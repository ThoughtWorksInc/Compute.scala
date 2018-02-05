package com.thoughtworks.compute

import java.util.{Collections, IdentityHashMap}
import java.util.concurrent.Callable

import com.dongxiguo.fastring.Fastring.Implicits._
import com.google.common.cache._
import com.thoughtworks.continuation._
import com.thoughtworks.compute.Expressions.{Arrays, Floats}
import com.thoughtworks.compute.OpenCLKernelBuilder.GlobalContext
import com.thoughtworks.compute.Trees.{FloatArrayTrees, StructuralTrees}
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import org.apache.commons.math3.linear._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.language.existentials
import scalaz.Tags.Parallel
import scalaz.std.list._
import scalaz.syntax.all._
import scalaz.syntax.tag._

// TODO: Rename to VirtualTensors, like virtual-dom
trait Tensors extends OpenCL {

  def concatenate(tensors: Seq[Tensor], dimension: Int): Tensor = ???

  protected val trees: FloatArrayTrees with StructuralTrees { type Category = Floats with Arrays } =
    Factory[FloatArrayTrees with StructuralTrees].newInstance()
  import trees._

  private def upvalues(tree: TreeApi): List[Parameter] = {
    val traversed: java.util.Set[TreeApi] = Collections.newSetFromMap(new IdentityHashMap)
    val builder = List.newBuilder[Parameter]
    def buildParameterList(tree: TreeApi): Unit = {
      tree match {
        case tree: Parameter =>
          builder += tree
        case _ =>
          val productArity = tree.productArity
          @tailrec def loop(i: Int): Unit = {
            if (i < productArity) {
              tree.productElement(i) match {
                case child: TreeApi =>
                  val isNew = traversed.add(tree)
                  if (isNew) {
                    buildParameterList(child)
                  }
                case _ =>
              }
              loop(i + 1)
            }
          }
          loop(0)
      }

    }
    buildParameterList(tree)
    builder.result()
  }

  // The scalar data type is hard-coded Float at the moment. FIXME: Allow other types in the future
  trait PendingBuffer {
    def event: Event

    def buffer: DeviceBuffer[Float]

    def toHostBuffer = {
      // We want to read buffer after `event` complete.
      // According to OpenCL's specification, we should pass the `event` as an `event_wait_list` to `clEnqueueReadBuffer`.
      // Unfortunately it seems not work on AMD SDK
      // buffer.toHostBuffer(event)
      //
      // Instead, we have to wait the event then call `clEnqueueReadBuffer`:
      Do.garbageCollected(event.waitForComplete()) >> buffer.toHostBuffer(event)

      // TODO: find other solution instead of this workaround
    }
  }
  object Tensor {
    def fill(value: Float, shape0: Array[Int], padding: Float = 0.0f) = {
      val padding0 = padding
      new InlineTensor {
        val padding: Float = padding0
        val shape: shape0.type = shape0
        val closure: trees.FloatTerm = float.literal(value)
      }
    }
  }

  sealed trait Tensor { thisTensor =>
    def broadcast(newShape: Array[Int]): Tensor = ???

    def *(rightHandSide: Tensor): Tensor = ???

    def +(rightHandSide: Tensor): Tensor = ???

    def translate(
        offset: Array[Double],
        newShape: Array[Int] = shape) /*(implicit debuggingInformation0: Implicitly[DebuggingInformation])*/: Tensor = {
      if (offset.length != thisTensor.shape.length) {
        throw new IllegalArgumentException
      }

      thisTensor match {
        case thisTensor: TransformedTensor =>
          new TransformedTensor {
            val matrix: RealMatrix = {
              val newMatrix = thisTensor.matrix.copy()
              for (i <- offset.indices) {
                newMatrix.addToEntry(i, newMatrix.getColumnDimension - 1, offset(i))
              }
              newMatrix
            }
            val checkpoint: Tensor = thisTensor.checkpoint
            val shape: Array[Int] = thisTensor.shape
            //          val debuggingInformation: Implicitly[DebuggingInformation] = debuggingInformation0
            val padding: Float = thisTensor.padding
          }
        case _ =>
          val newMatrix = MatrixUtils.createRealMatrix(shape.length, shape.length + 1)
          for (i <- offset.indices) {
            newMatrix.setEntry(i, i, 1.0)
            newMatrix.setEntry(i, newMatrix.getColumnDimension - 1, offset(i))
          }
          new TransformedTensor {
            def checkpoint: Tensor = thisTensor
            def shape: Array[Int] = newShape
            //          val debuggingInformation: Implicitly[DebuggingInformation] = debuggingInformation0
            def matrix: RealMatrix = newMatrix

            def padding: Float = checkpoint.padding
          }
      }
    }

    def split(dimension: Int): Seq[Tensor] = ???

    //    def debuggingInformation: Implicitly[DebuggingInformation]

    def shape: Array[Int]

    def closure: ValueTerm

    // TODO: rename to make buffer
    def enqueue: Do[PendingBuffer]

    def padding: Float

  }

  trait CompiledKernel extends MonadicCloseable[UnitContinuation] {
    def run(parameters: List[Parameter]): Do[PendingBuffer]
  }

  protected def kernelCacheBuilder: CacheBuilder[ValueTerm, CompiledKernel] = {
    CacheBuilder
      .newBuilder()
      .removalListener(new RemovalListener[ValueTerm, CompiledKernel] {
        def onRemoval(notification: RemovalNotification[ValueTerm, CompiledKernel]): Unit = {
          val compiledKernel = notification.getValue
          compiledKernel.monadicClose.blockingAwait
        }
      })
  }

  protected[compute] val kernelCache: Cache[ValueTerm, CompiledKernel] = kernelCacheBuilder.build()

  protected implicit val executionContext: ExecutionContext

  private def clearCache: UnitContinuation[Unit] = UnitContinuation.execute {
    kernelCache.invalidateAll()
    kernelCache.cleanUp()
  }

  override def monadicClose: UnitContinuation[Unit] = {
    clearCache >> super.monadicClose
  }

  /** An intermediate expression of tensor that can be composed into a more complex expression.
    *
    * @note When this [[InlineTensor]] is referenced more than one expressions,
    *       the computation for the tensor may be evaluated more than once.
    * @see [[force]] to create a tensor that will cache the result.
    */
  trait InlineTensor extends Tensor {
    def force: BufferedTensor = {
      new {
//        val debuggingInformation: Implicitly[DebuggingInformation] = InlineTensor.this.debuggingInformation
        val shape: Array[Int] = InlineTensor.this.shape
        val enqueue: Do[PendingBuffer] = InlineTensor.this.enqueue
        val padding: Float = InlineTensor.this.padding
      } with BufferedTensor
    }

    lazy val enqueue: Do[PendingBuffer] = {
      val compiledKernel = kernelCache.getIfPresent(closure) match {
        case null =>
          val alphConversionContext = new AlphaConversionContext
          val convertedTree = closure.tree.alphaConversion(alphConversionContext)
          val loader = new Callable[CompiledKernel] {
            def call(): CompiledKernel = {
              val sourceCode = {
                val globalContext = new GlobalContext
                val functionContext = Factory[OpenCLKernelBuilder].newInstance(globalContext)

                val exportContext = new ExportContext
                val kernelBody = convertedTree.export(functionContext, exportContext).asInstanceOf[functionContext.Term]

                val kernelParameters = upvalues(closure.tree).map { upvalue: Parameter =>
                  exportContext.get(alphConversionContext.get(upvalue)).asInstanceOf[functionContext.Term]
                }
                fastraw"""
              $globalContext
              ${functionContext.generateKernelSourceCode("jit_kernel", shape.length, kernelParameters, Seq(kernelBody))}
              """
              }

              sourceCode.foreach(print)

              val program = createProgramWithSource(sourceCode)
              program.build()

              val compiledKernel = new CompiledKernel {

                def monadicClose: UnitContinuation[Unit] = program.monadicClose

                def run(upvalues: List[Parameter]): Do[PendingBuffer] = {
                  // TODO: Manage life cycle of upvalues more delicately
                  // e.g. a buffer should be release as soon as possible if it is a dependency of another buffer,
                  // e.g. however, it can be hold longer time if it is dependencies of many other buffers.

                  upvalues
                    .traverse[ParallelDo, PendingBuffer] { tree =>
                      Parallel(tree.id.asInstanceOf[Tensor].enqueue)
                    }
                    .unwrap
                    .intransitiveFlatMap { arguments: List[PendingBuffer] =>
                      Do.monadicCloseable(program.createFirstKernel()).intransitiveFlatMap { kernel: Kernel =>
                        allocateBuffer[Float](shape.product).flatMap { outputBuffer =>
                          for ((arugment, i) <- arguments.view.zipWithIndex) {
                            kernel(i) = arugment.buffer
                          }
                          kernel(arguments.length) = outputBuffer
                          kernel.enqueue(shape.view.map(_.toLong): _*).map { event0 =>
                            new PendingBuffer {
                              val event: Event = event0
                              val buffer: DeviceBuffer[Float] = outputBuffer
                            }
                          }
                        }
                      }
                    }

                }
              }
              compiledKernel
            }
          }
          kernelCache.get(float.factory.newInstance(convertedTree.asInstanceOf[float.Tree]), loader)
        case compiledKernel =>
          compiledKernel
      }

      compiledKernel.run(upvalues(closure.tree)).shared
    }
  }

  trait TransformedTensor extends InlineTensor {

    def checkpoint: Tensor

    /** A matrix that describes the transformation of coordinate.
      *
      * The matrix size is __number of dimensions of original tensor × number of dimensions of new tensor__.
      */
    def matrix: RealMatrix

    val closure: ValueTerm = {
      array.parameter(checkpoint, float, padding, shape).transform(matrix).extract
    }
  }

  trait BufferedTensor extends Tensor {
    val closure: ValueTerm = {
      array.parameter(this, float, padding, shape).extract
    }
  }

}
