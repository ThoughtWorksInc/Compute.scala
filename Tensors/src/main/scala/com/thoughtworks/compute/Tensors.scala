package com.thoughtworks.compute

import java.util.{Collections, IdentityHashMap}
import java.util.concurrent.Callable

import com.dongxiguo.fastring.Fastring.Implicits._
import com.google.common.cache._
import com.thoughtworks.continuation._
import com.thoughtworks.compute.Expressions.{Pointers, Floats}
import com.thoughtworks.compute.NDimensionalAffineTransform.MatrixData
import com.thoughtworks.compute.OpenCLKernelBuilder.GlobalContext
import com.thoughtworks.compute.Trees.{FloatPointerTrees, StructuralTrees}
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._

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

  protected val trees: FloatPointerTrees with StructuralTrees { type Category = Floats with Pointers } =
    Factory[FloatPointerTrees with StructuralTrees].newInstance()
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
      buffer.toHostBuffer(event)
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
    def broadcast(newShape: Array[Int]): Tensor = {
      val newLength = newShape.length
      val length = shape.length
      val matrix1 = Array.ofDim[Double]((newLength + 1) * length)

      @tailrec
      def loop(i: Int): Unit = {
        if (i < length) {
          shape(i) match {
            case di if di == newShape(i) =>
              matrix1(i * (length + 1) + i) = 1.0
            case 1 =>
            case _ =>
              throw new IllegalArgumentException(
                raw"""Cannot broadcast ${shape.mkString("[", ",", "]")} to ${newShape.mkString("[", ",", "]")}""")
          }
          loop(i + 1)
        }
      }
      loop(0)

      transform(newShape, matrix1)
    }

    private def derivedTensor(newClosure: FloatTerm): InlineTensor = {
      new {
        val padding: Float = thisTensor.padding
        val shape: Array[Int] = thisTensor.shape
        val closure: ValueTerm = newClosure
      } with InlineTensor
    }

    def unary_- : Tensor = {
      derivedTensor(closure.asInstanceOf[FloatTerm].unary_-)
    }

    def unary_+ : Tensor = this

    def *(rightHandSide: Tensor): Tensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] * rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def +(rightHandSide: Tensor): Tensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] + rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def -(rightHandSide: Tensor): Tensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] - rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def /(rightHandSide: Tensor): Tensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] / rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def %(rightHandSide: Tensor): Tensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] % rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def scale(newShape: Array[Int]): Tensor = {
      val length = newShape.length
      if (length != shape.length) {
        throw new IllegalArgumentException
      }
      val matrix1 = Array.ofDim[Double](length * (length + 1))
      @tailrec
      def loop(i: Int): Unit = {
        if (i < length) {
          matrix1(i * (length + 1) + i) = shape(i).toDouble / newShape(i)
          loop(i + 1)
        }
      }
      loop(0)
      transform(newShape, matrix1)
    }

    def translate(offset: Array[Double], newShape: Array[Int] = shape): Tensor = {
      if (offset.length != thisTensor.shape.length) {
        throw new IllegalArgumentException
      }
      val translateMatrix = NDimensionalAffineTransform.translate(offset.map(-_))
      transform(newShape, translateMatrix)
    }

    private def transform(newShape: Array[Int], matrix1: MatrixData): TransformedTensor = {
      thisTensor match {
        case thisTensor: TransformedTensor =>
          new TransformedTensor {
            val matrix: MatrixData = {
              NDimensionalAffineTransform.concatenate(thisTensor.matrix, matrix1, thisTensor.shape.length)
            }
            val checkpoint: Tensor = thisTensor.checkpoint
            val shape: Array[Int] = thisTensor.shape
            //          val debuggingInformation: Implicitly[DebuggingInformation] = debuggingInformation0
            val padding: Float = thisTensor.padding
          }
        case _ =>
          new TransformedTensor {
            def checkpoint: Tensor = thisTensor

            def shape: Array[Int] = newShape

            //          val debuggingInformation: Implicitly[DebuggingInformation] = debuggingInformation0
            def matrix: MatrixData = matrix1

            def padding: Float = checkpoint.padding
          }
      }
    }

    def permute(dimensions: Array[Int]): TransformedTensor = {
      val length = shape.length
      if (dimensions.length != length) {
        throw new IllegalArgumentException
      }
      val newShape = Array.ofDim[Int](length)
      val matrix = Array.ofDim[Double](length * (length + 1))
      @tailrec def loop(newDimensionIndex: Int): Unit = {
        if (newDimensionIndex < length) {
          val oldDimensionIndex = dimensions(newDimensionIndex)
          newShape(newDimensionIndex) = shape(oldDimensionIndex)
          matrix(oldDimensionIndex * (length + 1) + newDimensionIndex) = 1.0
          loop(newDimensionIndex + 1)
        }
      }
      loop(0)
      transform(newShape, matrix)
    }

    def split(dimension: Int): IndexedSeq[Tensor] = {
      // TODO: override map/reduce to produce less OpenCL C code
      new IndexedSeq[Tensor] {

        private val newShape = shape.patch(dimension, Nil, 1)

        val length: Int = shape(dimension)

        def apply(index: Int): TransformedTensor = {
          val length = shape.length
          val matrix = Array.ofDim[Double](length * length)

          @tailrec
          def loopBefore(i: Int): Unit = {
            if (i < dimension) {
              matrix(i * length + i) = 1.0
              loopBefore(i + 1)
            }
          }
          loopBefore(0)

          matrix(dimension * length + length - 1) = index.toDouble

          @tailrec
          def loopAfter(i: Int): Unit = {
            if (i < length) {
              matrix(i * length + i - 1) = 1.0
              loopAfter(i + 1)
            }
          }
          loopAfter(dimension + 1)

          // TODO: Cache the transformed tensor
          transform(newShape, matrix)
        }

      }
    }

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
    * @see [[bufferred]] to create a tensor that will cache the result.
    */
  trait InlineTensor extends Tensor {
    def buffered: BufferedTensor = {
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
                          kernel
                            .enqueue(globalWorkSize = shape.view.map(_.toLong),
                                     waitingEvents = arguments.view.map(_.event.handle))
                            .map { event0 =>
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
    def matrix: MatrixData

    val closure: ValueTerm = {
      pointer.parameter(checkpoint, float, padding, shape).transform(matrix).extract
    }
  }

  trait BufferedTensor extends Tensor {
    val closure: ValueTerm = {
      pointer.parameter(this, float, padding, shape).extract
    }
  }

}
