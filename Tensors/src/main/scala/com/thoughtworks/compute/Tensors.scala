package com.thoughtworks.compute

import java.nio.{ByteBuffer, FloatBuffer}
import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicReference
import java.util.{Collections, IdentityHashMap}

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import com.github.ghik.silencer.silent
import com.google.common.cache._
import com.thoughtworks.compute.Expressions.{Arrays, Floats, Tuples}
import com.thoughtworks.compute.NDimensionalAffineTransform.MatrixData
import com.thoughtworks.compute.OpenCLKernelBuilder.GlobalContext
import com.thoughtworks.compute.Tensors.{MemoryTrees, TensorBuilder}
import com.thoughtworks.compute.Trees.{AllTrees, StructuralTrees}
import com.thoughtworks.continuation._
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import com.thoughtworks.tryt.covariant.TryT
import org.lwjgl.opencl.CL10.CL_DEVICE_TYPE_CPU
import org.lwjgl.system.MemoryUtil
import scalaz.Free.Trampoline
import shapeless.Witness

import scala.annotation.meta.companionObject
import scala.annotation.tailrec
import scala.collection.{SeqView, mutable}
import scala.concurrent.{ExecutionContext, SyncVar}
import scala.language.existentials
import scala.reflect.ClassTag
import scala.util.{Random, Success}
import scalaz.Tags.Parallel
import scalaz.Trampoline
import scalaz.std.list._
import scalaz.syntax.all._
import scalaz.syntax.tag._

object Tensors {

  /** A plug-in of Tensors to suppress warnings during compiling a OpenCL kernel for non-AMD platforms. */
  trait SuppressWarnings extends Tensors {
    @transient
    private lazy val _openclCompilerFlags = {
      if (platformCapabilities.cl_amd_compile_options) {
        // AMD SDK does not support -w flag in OpenCL specification.
        super.openclCompilerFlags
      } else {
        super.openclCompilerFlags + " -w"
      }
    }

    override protected def openclCompilerFlags: String = _openclCompilerFlags
  }

  trait UnsafeMathOptimizations extends Tensors {
    private lazy val _openclCompilerFlags = super.openclCompilerFlags + " -cl-unsafe-math-optimizations"
    override protected def openclCompilerFlags: String = _openclCompilerFlags
  }

  trait TensorBuilder[Data] {
    type Element
    def flatten(a: Data): Seq[Element]
    def shape(a: Data): Seq[Int]
  }

  private[Tensors] trait LowPriorityTensorBuilder {

    implicit def tensorBuilder0[Data]: TensorBuilder.Aux[Data, Data] = {
      new TensorBuilder[Data] {
        type Element = Data

        def flatten(a: Data): Seq[Data] = Seq(a)

        def shape(a: Data): Seq[Int] = Nil
      }
    }

  }
  object TensorBuilder extends LowPriorityTensorBuilder {
    type Aux[Data, Element0] = TensorBuilder[Data] {
      type Element = Element0
    }

    implicit def nDimensionalSeqToNDimensionalSeq[Data, Nested, Element0](
        implicit asSeq: Data => Seq[Nested],
        nestedBuilder: TensorBuilder.Aux[Nested, Element0]): TensorBuilder.Aux[Data, Element0] = {
      new TensorBuilder[Data] {
        type Element = Element0

        def flatten(a: Data): Seq[Element] = a.flatMap { nested =>
          nestedBuilder.flatten(nested)
        }

        def shape(a: Data): Seq[Int] = {
          val nestedSeq = asSeq(a)
          if (nestedSeq.isEmpty) {
            0 :: Nil
          } else {
            nestedSeq.length +: nestedSeq.map(nestedBuilder.shape).reduce { (a0, a1) =>
              if (a0 == a1) {
                a0
              } else {
                throw new IllegalArgumentException
              }
            }
          }
        }
      }
    }
  }

  private val ScalarShape: Array[Int] = Array.empty[Int]

  trait LcgRandomNumberGenerator extends Tensors {
    protected def hashSourceCode: Fastring = fastraw"""
      static inline uint hash(uint value) {
        return 1103515245 * value + 12345;
      }
    """
  }

  trait WangHashingRandomNumberGenerator extends Tensors {
    protected def hashSourceCode: Fastring = fastraw"""
      static inline uint hash(uint value) {
        value = (value ^ 61) ^ (value >> 16);
        value *= 9;
        value ^= value << 4;
        value *= 0x27d4eb2d;
        value ^= value >> 15;
        return value;
      }
    """
  }

  private[Tensors] final class ArrayMemory[Element](length: Int)(implicit val elementMemory: Memory[Element],
                                                                 classTag: ClassTag[Element])
      extends Memory[Array[Element]] {
    type HostBuffer = elementMemory.HostBuffer

    def fromByteBuffer(byteBuffer: ByteBuffer): HostBuffer = elementMemory.fromByteBuffer(byteBuffer)

    def numberOfBytesPerElement: Int = elementMemory.numberOfBytesPerElement * length

    def address(buffer: HostBuffer): Long = elementMemory.address(buffer)

    def remaining(buffer: HostBuffer): Int = elementMemory.remaining(buffer) / length

    def get(buffer: HostBuffer, index: Int): Array[Element] = {
      val array = Array.ofDim[Element](length)
      val start = index * length
      @tailrec
      def loop(i: Int): Unit = {
        if (i < length) {
          array(i) = elementMemory.get(buffer, start + i)
          loop(i + 1)
        }
      }
      loop(0)
      array
    }

    def put(buffer: HostBuffer, index: Int, value: Array[Element]): Unit = {
      val start = index * length
      @tailrec
      def loop(i: Int): Unit = {
        if (i < length) {
          elementMemory.put(buffer, start + i, value(i))
          loop(i + 1)
        }
      }
      loop(0)
    }

    def allocate(numberOfElement: Int): HostBuffer = {
      elementMemory.allocate(numberOfElement * length)
    }

    def free(buffer: HostBuffer): Unit = {
      elementMemory.free(buffer)
    }

    def toArray(buffer: HostBuffer): Array[Array[Element]] = {
      elementMemory.toArray(buffer).grouped(length).toArray
    }
  }

  trait MemoryTrees extends AllTrees {
    protected trait MemoryValueType extends ValueTreeType {
      def memory: Memory[JvmValue]
      def classTag: ClassTag[JvmValue]

    }
    type ValueType <: (Type with Any) with MemoryValueType

    protected trait MemoryFloatType extends FloatTreeType with MemoryValueType {
      def classTag: ClassTag[Float] = scala.reflect.classTag[Float]
      def memory: Memory.FloatMemory.type = Memory.FloatMemory
    }
    type FloatType <: (ValueType with Any) with MemoryFloatType

    protected trait MemoryTupleType extends TupleTreeType with MemoryValueType { thisTupleType =>

      def classTag: ClassTag[JvmValue] = elementType.classTag.wrap.asInstanceOf[ClassTag[JvmValue]]

      val memory: Memory[JvmValue] =
        new ArrayMemory[elementType.JvmValue](length)(elementType.memory, elementType.classTag)
          .asInstanceOf[Memory[JvmValue]]
    }

    type TupleType <: (ValueType with Any) with MemoryTupleType

  }
}

trait Tensors extends OpenCL {

  protected val trees
    : AllTrees with MemoryTrees with StructuralTrees { type Category = Tuples with Floats with Arrays } =
    Factory[AllTrees with MemoryTrees with StructuralTrees].newInstance()

  import trees._

  private def parameterDescendants(tree: Tree): List[Parameter] = {
    val traversed: java.util.Set[Any] = Collections.newSetFromMap(new IdentityHashMap)
    val builder = List.newBuilder[Parameter]
    def appendParameters(node: Any): Unit = {
      val isNew = traversed.add(node)
      if (isNew) {
        node match {
          case parameter: Parameter @unchecked =>
            builder += parameter
          case tree: Tree =>
            tree.productIterator.foreach(appendParameters)
          case seq: Seq[_] =>
            seq.foreach(appendParameters)
          case array: Array[_] =>
            array.foreach(appendParameters)
          case _ =>
        }
      }
    }
    appendParameters(tree)
    builder.result()
  }

  protected sealed trait PendingBuffer[JvmType] {
    def buffer: DeviceBuffer[JvmType]
    def eventOption: Option[Event]
    def toHostBuffer()(implicit memory: Memory[JvmType]): Do[memory.HostBuffer]

    def toArray()(implicit memory: Memory[JvmType]): Do[Array[JvmType]] = {
      toHostBuffer()(memory).intransitiveMap { hostBuffer: memory.HostBuffer =>
        memory.toArray(hostBuffer)
      }
    }
  }

  @(silent @companionObject)
  protected final case class ReadyBuffer[JvmType](buffer: DeviceBuffer[JvmType]) extends PendingBuffer[JvmType] {
    def toHostBuffer()(implicit memory: Memory[JvmType]): Do[memory.HostBuffer] = {
      buffer.toHostBuffer()(Witness(Tensors.this), memory)
    }
    def eventOption = None
  }

  @(silent @companionObject)
  protected final case class EventBuffer[JvmType](buffer: DeviceBuffer[JvmType], event: Event)
      extends PendingBuffer[JvmType] {
    def eventOption = Some(event)
    def toHostBuffer()(implicit memory: Memory[JvmType]): Do[memory.HostBuffer] = {
      buffer.toHostBuffer(event)
    }
  }

  protected def hashSourceCode: Fastring

  protected def openclCompilerFlags: String = ""

  protected object PlusPrograms extends MonoidPrograms {
    def append(leftHandSide: Fastring, rightHandSide: Fastring): Fastring = fast"(($leftHandSide) + ($rightHandSide))"
    def zero: Fastring = fast"0.0f"
  }

  protected trait MonoidPrograms {
    def append(leftHandSide: Fastring, rightHandSide: Fastring): Fastring
    def zero: Fastring

    @transient
    lazy val sequentialReductionProgram = {
      val program = createProgramWithSource(fastraw"""
        static float reduce16(global const float * restrict buffer, const uint length) {
          float16 accumulator = vload16(get_global_id(0), buffer);
          for (uint global_index = get_global_id(0) + get_global_size(0);
               global_index < length;
               global_index += get_global_size(0)
          ) {
            accumulator = ${append(fast"accumulator", fast"vload16(global_index, buffer)")};
          }
          const float8 f8 = ${append(fast"accumulator.hi", fast"accumulator.lo")};
          const float4 f4 = ${append(fast"f8.hi", fast"f8.lo")};
          const float2 f2 = ${append(fast"f4.hi", fast"f4.lo")};
          return ${append(fast"f2.x", fast"f2.y")};
        }

        static float reduce_rest(global const float * restrict buffer, const uint length, float accumulator) {
          for (uint global_index = get_global_id(0); global_index < length; global_index += get_global_size(0)) {
            accumulator = ${append(fast"accumulator", fast"buffer[global_index]")};
          }
          return accumulator;
        }

        kernel void reduce(global const float * restrict buffer,
                           const uint length,
                           global float * restrict result) {
          const uint vector_length = length / 16;
          if (vector_length >= get_global_size(0)) {
            const int stage1_length = 16 * vector_length;
            const int stage2_length = length - stage1_length;
            result[get_global_id(0)] = reduce_rest(buffer + stage1_length, stage2_length, reduce16(buffer, vector_length));
          } else {
            result[get_global_id(0)] = reduce_rest(buffer, length, $zero);
          }
        }
      """)
      program.build(openclCompilerFlags)
      program
    }

    /**
      * @see https://stackoverflow.com/questions/20753862/why-nvidia-and-amd-opencl-reduction-example-did-not-reduce-an-array-to-an-elemen/49253218#49253218
      *      for preferred global size / local size settings.
      */
    @transient
    lazy val parallelReductionProgram = {
      val program = createProgramWithSource(fastraw"""
        kernel void reduce(global const float * restrict buffer,
                           local float * restrict local_scratch,
                           const int length,
                           global float * restrict result) {
          int global_index = get_global_id(0);
          float accumulator = $zero;
          // Loop sequentially over chunks of input vector
          while (global_index < length) {
            float element = buffer[global_index];
            accumulator = ${append(fast"accumulator", fast"element")};
            global_index += get_global_size(0);
          }
          // Perform parallel reduction in a work group
          local_scratch[get_local_id(0)] = accumulator;
          barrier(CLK_LOCAL_MEM_FENCE);
          for (uint offset = get_local_size(0) / 2;
               offset > 1;
               offset = offset / 2) {
            if (get_local_id(0) < offset) {
              const float other = local_scratch[get_local_id(0) + offset];
              const float mine = local_scratch[get_local_id(0)];
              local_scratch[get_local_id(0)] = ${append(fast"mine", fast"other")};
            }
            barrier(CLK_LOCAL_MEM_FENCE);
          }
          if (get_local_id(0) == 0) {
            result[get_group_id(0)] = ${append(fast"local_scratch[0]", fast"local_scratch[1]")};
          }
        }
      """)
      program.build(openclCompilerFlags)
      program
    }
  }

  object Tensor {

    @transient
    private lazy val randomNormalProgram: Program = {

      val program = createProgramWithSource(fastraw"""
        $hashSourceCode

        static inline uint xorshift(uint seed) {
          const uint tmp1 = seed ^ (seed << 13);
          const uint tmp2 = tmp1 ^ (tmp1 >> 17);
          return tmp2 ^ (tmp2 << 5);
        }

        kernel void random_normal(global float * const restrict buffer, const uint seed) {
          const uint i = get_global_id(0);
          const uint r1 = hash(i ^ seed);
          const uint r2 = xorshift(r1);
          const float u1 = r1 / 4294967296.0f;
          const float u2 = r2 / 4294967296.0f;

          const float r = sqrt(-2 * log(u1));
          const float theta = 2 * M_PI_F * u2;

          const float z0 = r * cos(theta);
          const float z1 = r * sin(theta);


          buffer[i * 2] = z0;
          buffer[i * 2 + 1] = z1;
        }
      """)
      program.build(openclCompilerFlags)
      program
    }

    @transient
    private lazy val randomProgram: Program = {
      val program = createProgramWithSource(fastraw"""
    $hashSourceCode

    kernel void random(global float * const restrict buffer, const uint seed) {
      const uint i = get_global_id(0);
      buffer[i] = hash(i ^ seed) / 4294967296.0f;
    }
    """)
      program.build(openclCompilerFlags)
      program
    }

    def apply[A](elements: A, padding: Float = 0.0f)(
        implicit tensorBuilder: TensorBuilder.Aux[A, Float]): NonInlineTensor = {
      val padding0 = padding
      new {
        val shape: Array[Int] = tensorBuilder.shape(elements).toArray
        val padding: Float = padding0
      } with NonInlineTensor {
        private[compute] val doBuffer = {
          Do(TryT(ResourceT(UnitContinuation.delay {
            val data = tensorBuilder.flatten(elements).toArray
            val hostBuffer = MemoryUtil.memAllocFloat(data.length)
            hostBuffer.duplicate().put(data)
            Resource(value = Success(hostBuffer), release = UnitContinuation.delay { MemoryUtil.memFree(hostBuffer) })
          }))).intransitiveFlatMap { hostBuffer =>
            allocateBufferFrom(hostBuffer).map(ReadyBuffer[Float]).asInstanceOf[Do[PendingBuffer[closure.JvmValue]]]
          }.shared
        }
      }
    }

    def scalar(value: Float, padding: Float = 0.0f): InlineTensor = {
      fill(value, Tensors.ScalarShape, padding)
    }

    def fill(value: Float, shape0: Array[Int], padding: Float = 0.0f): InlineTensor = {
      val padding0 = padding
      new {
        val padding: Float = padding0
        val shape: shape0.type = shape0
        val closure: trees.FloatTerm = float.literal(value)
      } with InlineTensor
    }

    def random(shape: Array[Int], seed: Int = Random.nextInt(), padding: Float = 0.0f): NonInlineTensor = {
      val shape0 = shape
      val padding0 = padding
      new {
        val padding = padding0
        val shape = shape0
      } with NonInlineTensor {
        private[compute] val doBuffer: Do[PendingBuffer[Float]] = {
          val size = shape.product
          allocateBuffer[Float](size).flatMap { buffer =>
            Do.monadicCloseable(randomProgram.createKernel()).intransitiveFlatMap { kernel =>
              kernel(0) = buffer
              kernel(1) = seed
              dispatch(kernel.enqueue(_, globalWorkSize = Array(size.toLong))).map(EventBuffer[Float](buffer, _))
            }
          }.shared
        }
      }
    }

    /** Generate random numbers in normal distribution. */
    def randomNormal(shape: Array[Int], seed: Int = Random.nextInt(), padding: Float = 0.0f): NonInlineTensor = {
      val shape0 = shape
      val padding0 = padding
      new {
        val padding = padding0
        val shape = shape0
      } with NonInlineTensor {
        private[compute] val doBuffer: Do[PendingBuffer[Float]] = {
          val size = shape.product
          val paddingSize = if (size % 2 == 1) {
            size + 1
          } else {
            size
          }
          allocateBuffer[Float](paddingSize).flatMap { buffer =>
            Do.monadicCloseable(randomNormalProgram.createKernel()).intransitiveFlatMap { kernel =>
              kernel(0) = buffer
              kernel(1) = seed
              val globalWorkSize = Array((paddingSize / 2).toLong)
              dispatch(kernel.enqueue(_, globalWorkSize = globalWorkSize)).map(EventBuffer[Float](buffer, _))
            }
          }
        }.shared
      }
    }

    def abs(leftHandSide: Tensor): InlineTensor = {
      leftHandSide.derivedTensor(trees.float.abs(leftHandSide.closure.asInstanceOf[FloatTerm]))
    }

    def sqrt(leftHandSide: Tensor): InlineTensor = {
      leftHandSide.derivedTensor(trees.float.sqrt(leftHandSide.closure.asInstanceOf[FloatTerm]))
    }

    def tanh(leftHandSide: Tensor): InlineTensor = {
      leftHandSide.derivedTensor(trees.float.tanh(leftHandSide.closure.asInstanceOf[FloatTerm]))
    }

    def exp(leftHandSide: Tensor): InlineTensor = {
      leftHandSide.derivedTensor(trees.float.exp(leftHandSide.closure.asInstanceOf[FloatTerm]))
    }

    def log(leftHandSide: Tensor): InlineTensor = {
      leftHandSide.derivedTensor(trees.float.log(leftHandSide.closure.asInstanceOf[FloatTerm]))
    }

    def min(leftHandSide: Tensor, rightHandSide: Tensor): InlineTensor = {
      def newClosure =
        trees.float.min(leftHandSide.closure.asInstanceOf[FloatTerm], rightHandSide.closure.asInstanceOf[FloatTerm])
      if (java.util.Arrays.equals(leftHandSide.shape, rightHandSide.shape)) {
        leftHandSide.derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def max(leftHandSide: Tensor, rightHandSide: Tensor): InlineTensor = {
      def newClosure =
        trees.float.max(leftHandSide.closure.asInstanceOf[FloatTerm], rightHandSide.closure.asInstanceOf[FloatTerm])
      if (java.util.Arrays.equals(leftHandSide.shape, rightHandSide.shape)) {
        leftHandSide.derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    def join(tensors0: Seq[Tensor]): NonInlineTensor = {
      def force[A](seq: Seq[A]) = {
        seq match {
          case seqView: SeqView[A, _] @unchecked =>
            seqView.force[A, Seq[A]](collection.breakOut)
          case _ =>
            seq
        }
      }
      val tensors = force(tensors0)
      val headTensor = tensors.head

      new {
        val shape = headTensor.shape :+ tensors.length
        val padding: Float = headTensor.padding
      } with NonInlineTensor {
        private[compute] val doBuffer = {
          val elements = tensors.map(_.closure)
          enqueueClosure(trees.tuple.join(elements: _*), headTensor.shape).asInstanceOf[Do[PendingBuffer[Float]]]
        }.shared
      }
    }

  }

  /**
    * @groupname metadata General information
    * @groupprio metadata 1
    * @groupdesc metadata
    *            Methods that provides general information of this [[Tensor]].
    *
    * @groupname slow Slow actions
    * @groupprio slow 2
    * @groupdesc slow
    *            Actions that can actually perform delayed operations
    *            in order to read the data from the device to JVM,
    *            or change the internal state of this [[Tensor]].
    *
    * @groupname delayed Delayed operators
    * @groupprio delayed 3
    * @groupdesc delayed
    *            Operators that return new [[Tensor]]s of delay-evaluated computational graphs.
    *            The actually computation will be only performed when '''Slow actions''' are called.
    *
    */
  sealed trait Tensor { thisTensor =>

    /**
      * @group delayed
      */
    def nonInline: NonInlineTensor

    private def reduce(programs: MonoidPrograms): NonInlineTensor = {
      new {
        val padding: Float = thisTensor.padding

        private[compute] val doBuffer: Do[PendingBuffer[Float]] = {
          thisTensor.doBuffer.intransitiveFlatMap { inputPendingBuffer: PendingBuffer[Float] =>
            val length = thisTensor.shape.product
            allocateBuffer[Float](1).flatMap { outputBuffer =>
              dispatch { commandQueue =>
                commandQueue.deviceId.deviceType match {
                  case CL_DEVICE_TYPE_CPU =>
                    Do.monadicCloseable(programs.sequentialReductionProgram.createKernel()).intransitiveFlatMap {
                      kernel1: Kernel =>
                        kernel1(0) = inputPendingBuffer.buffer
                        kernel1(1) = length
                        kernel1(2) = outputBuffer
                        kernel1
                          .enqueue(
                            commandQueue,
                            globalWorkSize = Array(1L),
                            localWorkSize = Some(Array(1L): Seq[Long]),
                            waitingEvents = inputPendingBuffer.eventOption.map(_.handle).toSeq
                          )
                    }
                  case _ =>
                    Do.monadicCloseable(programs.parallelReductionProgram.createKernel()).intransitiveFlatMap {
                      kernel1: Kernel =>
                        val stage1LocalWorkSize: Long = math.min(length, kernel1.workGroupSize(commandQueue.deviceId))
                        val maxNumberOfReductionGroups = commandQueue.deviceId.maxComputeUnits
                        val numberOfStage1Groups = if (length % stage1LocalWorkSize == 0) {
                          math.min(length / stage1LocalWorkSize, maxNumberOfReductionGroups)
                        } else {
                          math.min(length / stage1LocalWorkSize + 1, maxNumberOfReductionGroups)
                        }
                        if (numberOfStage1Groups == 1) {
                          kernel1(0) = inputPendingBuffer.buffer
                          kernel1.setLocalMemorySize[Float](1, stage1LocalWorkSize)
                          kernel1(2) = length
                          kernel1(3) = outputBuffer
                          kernel1.enqueue(
                            commandQueue,
                            globalWorkSize = Array(stage1LocalWorkSize),
                            localWorkSize = Some(Array(stage1LocalWorkSize): Seq[Long]),
                            waitingEvents = inputPendingBuffer.eventOption.map(_.handle).toSeq
                          )
                        } else {

                          allocateBuffer[Float](numberOfStage1Groups).intransitiveFlatMap { globalScratchBuffer =>
                            kernel1(0) = inputPendingBuffer.buffer
                            kernel1.setLocalMemorySize[Float](1, stage1LocalWorkSize)
                            kernel1(2) = length
                            kernel1(3) = globalScratchBuffer
                            kernel1
                              .enqueue(
                                commandQueue,
                                globalWorkSize = Array(stage1LocalWorkSize * numberOfStage1Groups),
                                localWorkSize = Some(Array(stage1LocalWorkSize): Seq[Long]),
                                waitingEvents = inputPendingBuffer.eventOption.map(_.handle).toSeq
                              )
                              .intransitiveFlatMap { scratchEvent: Event =>
                                Do.monadicCloseable(programs.parallelReductionProgram.createKernel())
                                  .intransitiveFlatMap { kernel2: Kernel =>
                                    // FIXME: An exception thrown here will not be handled. Need further investigation.

                                    val stage2WorkSize: Long =
                                      math.min(numberOfStage1Groups, kernel2.workGroupSize(commandQueue.deviceId))
                                    kernel2(0) = globalScratchBuffer
                                    kernel2.setLocalMemorySize[Float](1, stage2WorkSize)
                                    kernel2(2) = numberOfStage1Groups.toInt
                                    kernel2(3) = outputBuffer

                                    kernel2.enqueue(
                                      commandQueue,
                                      globalWorkSize = Array(stage2WorkSize),
                                      localWorkSize = Some(Array(stage2WorkSize): Seq[Long]),
                                      waitingEvents = Array(scratchEvent.handle)
                                    )
                                  }
                              }
                          }
                        }
                    }
                }

              }.map { stage2Event: Event =>
                EventBuffer[Float](outputBuffer, stage2Event)
              }
            }
          }
        }.shared
      } with NonInlineTensor {
        def shape: Array[Int] = Tensors.ScalarShape
      }
    }

    /**
      * @group delayed
      */
    def sum = reduce(PlusPrograms)

    /**
      * @group slow
      */
    override def toString: String = {
      doBuffer
        .intransitiveFlatMap { pendingBuffer =>
          pendingBuffer.toArray()(closure.valueType.memory)
        }
        .run
        .map { floatArray =>
          def toFastring(shape: Seq[Int], floatArray: Seq[closure.JvmValue]): Fastring = {
            shape match {
              case headSize +: tailShape =>
                val length = floatArray.length
                if (tailShape.isEmpty) {
                  if (headSize == length) {
                    fast"[${floatArray.mkFastring(",")}]"
                  } else {
                    throw new IllegalArgumentException
                  }
                } else {
                  val groupSize = length / headSize
                  def groups = for (i <- (0 until headSize).view) yield {
                    toFastring(tailShape, floatArray.view(i * groupSize, (i + 1) * groupSize))
                  }
                  fast"[${groups.mkFastring(",")}]"
                }
              case _ if floatArray.length == 1 =>
                fast"${floatArray(0)}"
              case _ =>
                throw new IllegalArgumentException(
                  raw"""shape${shape.mkString("(", ",", ")")} does not match the data size (${floatArray.length})""")
            }
          }

          toFastring(shape.view, floatArray).toString
        }
        .blockingAwait
    }

    /**
      * @group delayed
      */
    def broadcast(newShape: Array[Int]): TransformedTensor = {
      val newLength = newShape.length
      val length = shape.length
      val matrix1 = Array.ofDim[Double]((newLength + 1) * length)

      @tailrec
      def loop(i: Int): Unit = {
        if (i < length) {
          shape(i) match {
            case di if di == newShape(i) =>
              matrix1(i * (newLength + 1) + i) = 1.0
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
        val closure: FloatTerm = newClosure
      } with InlineTensor
    }

    /**
      * @group delayed
      */
    def reshape(newShape: Array[Int]): NonInlineTensor = {
      if (newShape.product != shape.product) {
        throw new IllegalArgumentException
      }
      new {
        val padding: Float = thisTensor.padding
        val shape: Array[Int] = newShape
        private[compute] val doBuffer: Do[PendingBuffer[Float]] = thisTensor.doBuffer
      } with NonInlineTensor
    }

    /**
      * @group delayed
      */
    def unary_- : InlineTensor = {
      derivedTensor(closure.asInstanceOf[FloatTerm].unary_-)
    }

    /**
      * @group delayed
      */
    def unary_+ : this.type = this

    /**
      * @group delayed
      */
    def *(rightHandSide: Tensor): InlineTensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] * rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    /**
      * @group delayed
      */
    def +(rightHandSide: Tensor): InlineTensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] + rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    /**
      * @group delayed
      */
    def -(rightHandSide: Tensor): InlineTensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] - rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    /**
      * @group delayed
      */
    def /(rightHandSide: Tensor): InlineTensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] / rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    /**
      * @group delayed
      */
    def %(rightHandSide: Tensor): InlineTensor = {
      def newClosure = thisTensor.closure.asInstanceOf[FloatTerm] % rightHandSide.closure.asInstanceOf[FloatTerm]
      if (java.util.Arrays.equals(shape, rightHandSide.shape)) {
        derivedTensor(newClosure)
      } else {
        throw new IllegalArgumentException
      }
    }

    /**
      * @group delayed
      */
    def scale(newShape: Array[Int]): TransformedTensor = {
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

    /**
      * @group delayed
      */
    def translate(offset: Array[Double], newShape: Array[Int] = shape): TransformedTensor = {
      if (offset.length != thisTensor.shape.length) {
        throw new IllegalArgumentException
      }
      val translateMatrix = NDimensionalAffineTransform.translate(offset.map(-_))
      transform(newShape, translateMatrix)
    }

    private def transform(newShape: Array[Int], matrix1: MatrixData): TransformedTensor = {
      thisTensor match {
        case thisTensor: TransformedTensor =>
          new {
            val matrix: MatrixData = {
              NDimensionalAffineTransform.preConcatenate(matrix1, thisTensor.matrix, newShape.length)
            }
            val checkpoint: Tensor = thisTensor.checkpoint
            val shape: Array[Int] = newShape
            //          val debuggingInformation: Implicitly[DebuggingInformation] = debuggingInformation0
            val padding: Float = thisTensor.padding
          } with TransformedTensor
        case _ =>
          new {
            val checkpoint: Tensor = thisTensor

            val shape: Array[Int] = newShape

            //          val debuggingInformation: Implicitly[DebuggingInformation] = debuggingInformation0
            val matrix: MatrixData = matrix1

          } with TransformedTensor {
            def padding: Float = checkpoint.padding
          }
      }
    }

    /**
      * @group delayed
      */
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

    /**
      * @group delayed
      */
    def split(dimension: Int): IndexedSeq[Tensor] = {
      // TODO: override map/reduce to produce less OpenCL C code
      val newShape = shape.patch(dimension, Nil, 1)
      new IndexedSeq[Tensor] {

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

    /**
      * Returns the sizes of each dimension of this [[Tensor]].
      *
      * @note The return value will be `Array.empty` if this [[Tensor]] is a scalar.
      * @note The returned array should be considered as immutable.
      *       Changing the array is an undefined behavior.
      * @group metadata
      */
    def shape: Array[Int]

    protected val closure: FloatTerm // FIXME: Allow element types other than float

    /** The back door to access [[closure]] for test purpose only.
      */
    private[compute] def getClosure = closure

    private[compute] def doBuffer: Do[PendingBuffer[closure.JvmValue]]

    /**
      * @group slow
      */
    def flatBuffer: Do[FloatBuffer] = {
      doBuffer.intransitiveFlatMap {
        _.toHostBuffer().map { hostBuffer =>
          val shapeProduct = shape.product
          if (hostBuffer.remaining() > shapeProduct) {
            hostBuffer.limit(hostBuffer.position() + shapeProduct)
          }
          hostBuffer
        }
      }
    }

    /**
      * @group slow
      */
    def flatArray: Future[Array[closure.JvmValue]] = {
      flatBuffer.intransitiveMap(closure.valueType.memory.toArray).run
    }

    /**
      * @group metadata
      */
    def padding: Float

    @transient
    private[Tensors] lazy val arrayTerm = {
      if (shape == null) {
        throw new IllegalArgumentException
      }

      array.parameter(this, float.literal(padding), shape)
    }
  }

  protected trait CompiledKernel extends MonadicCloseable[UnitContinuation] {
    def run(parameters: List[Parameter]): Do[PendingBuffer[_]]
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

  private def enqueueClosure(closure: ValueTerm, shape: Array[Int]): Do[PendingBuffer[closure.JvmValue]] =
    Do.suspend {
      val compiledKernel = kernelCache.getIfPresent(closure) match {
        case null =>
          val alphConversionContext = new AlphaConversionContext
          val convertedTerm: ValueTerm = closure.alphaConversion
          val loader = new Callable[CompiledKernel] {
            def call(): CompiledKernel = {
              val sourceCode = {
                val globalContext = new GlobalContext
                val functionContext = Factory[OpenCLKernelBuilder].newInstance(globalContext)

                val exportContext = new ExportContext

                val convertedTree = convertedTerm.tree
                val kernelBody = convertedTree.export(functionContext, exportContext)

                val upvalues = parameterDescendants(convertedTree)
                val kernelParameters = upvalues.map { upvalue: Parameter =>
                  val term = exportContext.get(upvalue)
                  assert(term != null)
                  term.asInstanceOf[functionContext.Term]
                }
                fastraw"""
                $globalContext
                ${functionContext.generateKernelSourceCode("jit_kernel",
                                                           shape.length,
                                                           kernelParameters,
                                                           Seq(kernelBody))}
                """
              }

              val program = createProgramWithSource(sourceCode)
              program.build(openclCompilerFlags)

              val compiledKernel = new CompiledKernel {

                def monadicClose: UnitContinuation[Unit] = program.monadicClose

                def run(upvalues: List[Parameter]): Do[PendingBuffer[convertedTerm.JvmValue]] = {
                  // TODO: Manage life cycle of upvalues more delicately
                  // e.g. a buffer should be release as soon as possible if it is a dependency of another buffer,
                  // e.g. however, it can be hold longer time if it is dependencies of many other buffers.

                  upvalues
                    .traverse[ParallelDo, PendingBuffer[_]] { tree =>
                      Parallel(tree.id.asInstanceOf[Tensor].doBuffer)
                    }
                    .unwrap
                    .intransitiveFlatMap { arguments: List[PendingBuffer[_]] =>
                      Do.monadicCloseable(program.createKernel()).intransitiveFlatMap { kernel: Kernel =>
                        val valueType = convertedTerm.valueType.asInstanceOf[ValueType]
                        val memory = valueType.memory.asInstanceOf[Memory[convertedTerm.JvmValue]]
                        allocateBuffer[convertedTerm.JvmValue](shape.product)(memory).flatMap { outputBuffer =>
                          for ((arugment, i) <- arguments.view.zipWithIndex) {
                            kernel(i) = arugment.buffer
                          }
                          kernel(arguments.length) = outputBuffer
                          dispatch(
                            kernel.enqueue(_,
                                           globalWorkSize = shape.view.map(_.toLong),
                                           waitingEvents = arguments.view.flatMap(_.eventOption.map(_.handle))))
                            .map(EventBuffer[convertedTerm.JvmValue](outputBuffer, _))
                        }
                      }
                    }

                }
              }
              compiledKernel
            }
          }
          kernelCache.get(convertedTerm, loader)
        case compiledKernel =>
          compiledKernel
      }

      compiledKernel.run(parameterDescendants(closure.tree)).asInstanceOf[Do[PendingBuffer[closure.JvmValue]]]
    }

  /** An intermediate expression of tensor that can be composed into a more complex expression. */
  trait InlineTensor extends Tensor { thisInlineTensor =>
    private[compute] val doBuffer: Do[PendingBuffer[closure.JvmValue]] = {
      enqueueClosure(closure, shape)
    }.shared

    def nonInline: NonInlineTensor =
      new {
        val padding: Float = thisInlineTensor.padding
        private[compute] val doBuffer: Do[PendingBuffer[Float]] = thisInlineTensor.doBuffer
        val shape: Array[Int] = thisInlineTensor.shape
      } with NonInlineTensor
  }

  trait TransformedTensor extends InlineTensor {

    def checkpoint: Tensor

    /** A matrix that describes the transformation of coordinate.
      *
      * The matrix size is __number of dimensions of original tensor × number of dimensions of new tensor__.
      */
    def matrix: MatrixData

    @transient
    protected lazy val closure: FloatTerm = {
      checkpoint.arrayTerm.transform(matrix).extract
    }

  }

  trait NonInlineTensor extends Tensor {

    def nonInline: this.type = this

    @transient
    protected lazy val closure = {
      arrayTerm.extract
    }

    /** Allocates device-side cache that are managed by the [[https://github.com/ThoughtWorksInc/RAII.scala RAII.scala]] library.
      *
      * @note This method is similar to [[cache]],
      *       except the life cycle of the cache can be automatically managed.
      *
      * @group slow
      */
    def doCache: Do[this.type] = doBuffer.map(Function.const(this))

    /** Allocates device-side cache for this [[Tensor]], and returns a [[java.lang.AutoCloseable]] to release the cache.
      *
      * @note This method can be called multiple times on one [[Tensor]].
      *       Only one copy of cache will be allocated,
      *       which will be finally released until all [[java.lang.AutoCloseable]] returned by [[cache]] method are closed.
      *
      * @group slow
      */
    def cache: AutoCloseable = {
      sealed trait State
      case object Openning extends State
      case object EarlyClosed extends State
      case object Closed extends State
      final case class Open(release: UnitContinuation[Unit]) extends State

      val state = new AtomicReference[State](Openning) with AutoCloseable {
        @tailrec
        final def close(): Unit = {
          get match {
            case Openning =>
              if (compareAndSet(Openning, EarlyClosed)) {
                // Success
              } else {
                close()
              }
            case oldState @ Open(release) =>
              if (compareAndSet(oldState, Closed)) {
                release.safeOnComplete { _: Unit =>
                  Trampoline.done(())
                }.run
              } else {
                close()
              }
            case EarlyClosed | Closed =>
              throw new IllegalStateException("The resources associated to this tensor has been released.")
          }
        }
      }

      doBuffer.safeOnComplete { resource =>
        @tailrec
        def retry(): Trampoline[Unit] = {
          state.get() match {
            case EarlyClosed =>
              if (state.compareAndSet(EarlyClosed, Closed)) {
                resource.release.safeOnComplete { _: Unit =>
                  Trampoline.done(())
                }
              } else {
                retry()
              }
            case Openning =>
              if (state.compareAndSet(Openning, Open(resource.release))) {
                Trampoline.done(())
              } else {
                retry()
              }
            case _: Open | Closed =>
              throw new IllegalStateException()
          }
        }
        retry()
      }.run

      state
    }

  }

}
