package com.thoughtworks.compute

import scala.collection.JavaConverters._
import java.nio.{ByteBuffer, IntBuffer}
import java.util.concurrent.{ConcurrentHashMap, Executors}
import java.util.concurrent.atomic.AtomicReference

import com.typesafe.scalalogging.{CanLog, Logger, StrictLogging}
import org.lwjgl.opencl._
import CL10._
import CL12._
import CL11._
import CL20._
import KHRICD._
import org.lwjgl.{BufferUtils, PointerBuffer}
import org.lwjgl.system.MemoryUtil._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.Pointer._

import scala.collection.mutable
import com.thoughtworks.each.Monadic._
import com.thoughtworks.compute.Memory.Box
import com.thoughtworks.compute.OpenCL.{Event, checkErrorCode}, Event.Status
import org.lwjgl.system.jni.JNINativeInterface
import org.lwjgl.system._

import scala.util.control.Exception.Catcher
import scala.util.control.{NonFatal, TailCalls}
import scala.util.control.TailCalls.TailRec
import scala.util.{Failure, Success, Try}
import scalaz.{-\/, Memo, \/, \/-}
import scalaz.syntax.all._
import com.thoughtworks.continuation._
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject
import com.thoughtworks.feature.mixins.ImplicitsSingleton
import com.thoughtworks.future._
import com.thoughtworks.raii.AsynchronousPool
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import com.thoughtworks.tryt.covariant._
import org.slf4j.MDC
import shapeless.Witness

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
object OpenCL {

  final case class PlatformId[Owner <: Singleton with OpenCL](handle: Long) extends AnyVal {

    final def deviceIdsByType(deviceType: Int): Seq[DeviceId[Owner]] = {
      val Array(numberOfDevices) = {
        val a = Array(0)
        checkErrorCode(clGetDeviceIDs(handle, deviceType, null, a))
        a
      }
      val stack = stackPush()
      try {
        val deviceIdBuffer = stack.mallocPointer(numberOfDevices)
        checkErrorCode(clGetDeviceIDs(handle, deviceType, deviceIdBuffer, null: IntBuffer))
        for (i <- 0 until numberOfDevices) yield {
          val deviceId = deviceIdBuffer.get(i)
          new DeviceId[Owner](deviceId)
        }
      } finally {
        stack.close()
      }
    }

  }

  /** Returns a [[String]] for the C string `address`.
    *
    * @note We don't know the exact charset of the C string. Use [[memASCII]] because lwjgl treats them as ASCII.
    */
  private def decodeString(address: Long): String = memASCII(address)

  /** Returns a [[String]] for the C string `byteBuffer`.
    *
    * @note We don't know the exact charset of the C string. Use [[memASCII]] because lwjgl treats them as ASCII.
    */
  private def decodeString(byteBuffer: ByteBuffer): String = memASCII(byteBuffer)

  @volatile
  var defaultLogger: (String, Option[ByteBuffer]) => Unit = { (errorInfo, data) =>
    // TODO: Add a test for in the case that Context is closed
    Console.err.println(raw"""An OpenCL notify comes out after its corresponding handler is freed
  message: $errorInfo
  data: $data""")
  }

  @deprecated(
    message = "[[finalize]] method should not be invoked by users.",
    since = "[[finalize]] is deprecated in Java 9. However, it is the only way to clean up static native resources."
  )
  override protected def finalize(): Unit = {
    contextCallback.close()
    super.finalize()
  }

  private val contextCallback: CLContextCallback = CLContextCallback.create(new CLContextCallbackI {
    def invoke(errInfo: Long, privateInfo: Long, size: Long, userData: Long): Unit = {
      val errorInfo = decodeString(errInfo)
      val dataOption = if (privateInfo != NULL) {
        Some(memByteBuffer(privateInfo, size.toInt))
      } else {
        None
      }
      memGlobalRefToObject[OpenCL](userData) match {
        case null =>
          defaultLogger(decodeString(errInfo), dataOption)
        case opencl =>
          if (size.isValidInt) {
            opencl.handleOpenCLNotification(decodeString(errInfo), dataOption)
          } else {
            throw new IllegalArgumentException(s"numberOfBytes($size) is too large")
          }
      }
    }
  })
  object Exceptions {
    final class MisalignedSubBufferOffset(message: String = null) extends IllegalArgumentException(message)

    final class ExecStatusErrorForEventsInWaitList(message: String = null) extends IllegalArgumentException(message)

    final class InvalidProperty(message: String = null) extends IllegalArgumentException(message)

    final class PlatformNotFoundKhr(message: String = null) extends NoSuchElementException(message)

    final class DeviceNotFound(message: String = null) extends NoSuchElementException(message)

    final class DeviceNotAvailable(message: String = null) extends IllegalStateException(message)

    final class CompilerNotAvailable(message: String = null) extends IllegalStateException(message)

    final class MemObjectAllocationFailure(message: String = null) extends IllegalStateException(message)

    final class OutOfResources(message: String = null) extends IllegalStateException(message)

    final class OutOfHostMemory(message: String = null) extends IllegalStateException(message)

    final class ProfilingInfoNotAvailable(message: String = null) extends IllegalStateException(message)

    final class MemCopyOverlap(message: String = null) extends IllegalStateException(message)

    final class ImageFormatMismatch(message: String = null) extends IllegalStateException(message)

    final class ImageFormatNotSupported(message: String = null) extends IllegalStateException(message)

    final class BuildProgramFailure[Owner <: Singleton with OpenCL](
        buildLogs: Map[DeviceId[Owner], String] = Map.empty[DeviceId[Owner], String])
        extends IllegalStateException({
          buildLogs.view
            .map {
              case (deviceId, buildLog) =>
                f"CL_BUILD_PROGRAM_FAILURE on device 0x${deviceId.handle}%X:\n$buildLog"
            }
            .mkString("\n")
        })

    final class MapFailure(message: String = null) extends IllegalStateException(message)

    final class InvalidValue(message: String = null) extends IllegalArgumentException(message)

    final class InvalidDeviceType(message: String = null) extends IllegalArgumentException(message)

    final class InvalidPlatform(message: String = null) extends IllegalArgumentException(message)

    final class InvalidDevice(message: String = null) extends IllegalArgumentException(message)

    final class InvalidContext(message: String = null) extends IllegalArgumentException(message)

    final class InvalidQueueProperties(message: String = null) extends IllegalArgumentException(message)

    final class InvalidCommandQueue(message: String = null) extends IllegalArgumentException(message)

    final class InvalidHostPtr(message: String = null) extends IllegalArgumentException(message)

    final class InvalidMemObject(message: String = null) extends IllegalArgumentException(message)

    final class InvalidImageFormatDescriptor(message: String = null) extends IllegalArgumentException(message)

    final class InvalidImageSize(message: String = null) extends IllegalArgumentException(message)

    final class InvalidSampler(message: String = null) extends IllegalArgumentException(message)

    final class InvalidBinary(message: String = null) extends IllegalArgumentException(message)

    final class InvalidBuildOptions(message: String = null) extends IllegalArgumentException(message)

    final class InvalidProgram(message: String = null) extends IllegalArgumentException(message)

    final class InvalidProgramExecutable(message: String = null) extends IllegalArgumentException(message)

    final class InvalidKernelName(message: String = null) extends IllegalArgumentException(message)

    final class InvalidKernelDefinition(message: String = null) extends IllegalArgumentException(message)

    final class InvalidKernel(message: String = null) extends IllegalArgumentException(message)

    final class InvalidArgIndex(message: String = null) extends IllegalArgumentException(message)

    final class InvalidArgValue(message: String = null) extends IllegalArgumentException(message)

    final class InvalidArgSize(message: String = null) extends IllegalArgumentException(message)

    final class InvalidKernelArgs(message: String = null) extends IllegalArgumentException(message)

    final class InvalidWorkDimension(message: String = null) extends IllegalArgumentException(message)

    final class InvalidWorkGroupSize(message: String = null) extends IllegalArgumentException(message)

    final class InvalidWorkItemSize(message: String = null) extends IllegalArgumentException(message)

    final class InvalidGlobalOffset(message: String = null) extends IllegalArgumentException(message)

    final class InvalidEventWaitList(message: String = null) extends IllegalArgumentException(message)

    final class InvalidEvent(message: String = null) extends IllegalArgumentException(message)

    final class InvalidOperation(message: String = null) extends IllegalArgumentException(message)

    final class InvalidBufferSize(message: String = null) extends IllegalArgumentException(message)

    final class InvalidGlobalWorkSize(message: String = null) extends IllegalArgumentException(message)

    final class UnknownErrorCode(errorCode: Int) extends IllegalStateException(s"Unknown error code: $errorCode")

    def fromErrorCode(errorCode: Int): Exception = errorCode match {
      case CL_PLATFORM_NOT_FOUND_KHR                    => new Exceptions.PlatformNotFoundKhr
      case CL_DEVICE_NOT_FOUND                          => new Exceptions.DeviceNotFound
      case CL_DEVICE_NOT_AVAILABLE                      => new Exceptions.DeviceNotAvailable
      case CL_COMPILER_NOT_AVAILABLE                    => new Exceptions.CompilerNotAvailable
      case CL_MEM_OBJECT_ALLOCATION_FAILURE             => new Exceptions.MemObjectAllocationFailure
      case CL_OUT_OF_RESOURCES                          => new Exceptions.OutOfResources
      case CL_OUT_OF_HOST_MEMORY                        => new Exceptions.OutOfHostMemory
      case CL_PROFILING_INFO_NOT_AVAILABLE              => new Exceptions.ProfilingInfoNotAvailable
      case CL_MEM_COPY_OVERLAP                          => new Exceptions.MemCopyOverlap
      case CL_IMAGE_FORMAT_MISMATCH                     => new Exceptions.ImageFormatMismatch
      case CL_IMAGE_FORMAT_NOT_SUPPORTED                => new Exceptions.ImageFormatNotSupported
      case CL_BUILD_PROGRAM_FAILURE                     => new Exceptions.BuildProgramFailure
      case CL_MAP_FAILURE                               => new Exceptions.MapFailure
      case CL_INVALID_VALUE                             => new Exceptions.InvalidValue
      case CL_INVALID_DEVICE_TYPE                       => new Exceptions.InvalidDeviceType
      case CL_INVALID_PLATFORM                          => new Exceptions.InvalidPlatform
      case CL_INVALID_DEVICE                            => new Exceptions.InvalidDevice
      case CL_INVALID_CONTEXT                           => new Exceptions.InvalidContext
      case CL_INVALID_QUEUE_PROPERTIES                  => new Exceptions.InvalidQueueProperties
      case CL_INVALID_COMMAND_QUEUE                     => new Exceptions.InvalidCommandQueue
      case CL_INVALID_HOST_PTR                          => new Exceptions.InvalidHostPtr
      case CL_INVALID_MEM_OBJECT                        => new Exceptions.InvalidMemObject
      case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR           => new Exceptions.InvalidImageFormatDescriptor
      case CL_INVALID_IMAGE_SIZE                        => new Exceptions.InvalidImageSize
      case CL_INVALID_SAMPLER                           => new Exceptions.InvalidSampler
      case CL_INVALID_BINARY                            => new Exceptions.InvalidBinary
      case CL_INVALID_BUILD_OPTIONS                     => new Exceptions.InvalidBuildOptions
      case CL_INVALID_PROGRAM                           => new Exceptions.InvalidProgram
      case CL_INVALID_PROGRAM_EXECUTABLE                => new Exceptions.InvalidProgramExecutable
      case CL_INVALID_KERNEL_NAME                       => new Exceptions.InvalidKernelName
      case CL_INVALID_KERNEL_DEFINITION                 => new Exceptions.InvalidKernelDefinition
      case CL_INVALID_KERNEL                            => new Exceptions.InvalidKernel
      case CL_INVALID_ARG_INDEX                         => new Exceptions.InvalidArgIndex
      case CL_INVALID_ARG_VALUE                         => new Exceptions.InvalidArgValue
      case CL_INVALID_ARG_SIZE                          => new Exceptions.InvalidArgSize
      case CL_INVALID_KERNEL_ARGS                       => new Exceptions.InvalidKernelArgs
      case CL_INVALID_WORK_DIMENSION                    => new Exceptions.InvalidWorkDimension
      case CL_INVALID_WORK_GROUP_SIZE                   => new Exceptions.InvalidWorkGroupSize
      case CL_INVALID_WORK_ITEM_SIZE                    => new Exceptions.InvalidWorkItemSize
      case CL_INVALID_GLOBAL_OFFSET                     => new Exceptions.InvalidGlobalOffset
      case CL_INVALID_EVENT_WAIT_LIST                   => new Exceptions.InvalidEventWaitList
      case CL_INVALID_EVENT                             => new Exceptions.InvalidEvent
      case CL_INVALID_OPERATION                         => new Exceptions.InvalidOperation
      case CL_INVALID_BUFFER_SIZE                       => new Exceptions.InvalidBufferSize
      case CL_INVALID_GLOBAL_WORK_SIZE                  => new Exceptions.InvalidGlobalWorkSize
      case CL_MISALIGNED_SUB_BUFFER_OFFSET              => new Exceptions.MisalignedSubBufferOffset
      case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST => new Exceptions.ExecStatusErrorForEventsInWaitList
      case CL_INVALID_PROPERTY                          => new Exceptions.InvalidProperty

      case _ => new Exceptions.UnknownErrorCode(errorCode)

    }

  }

  def checkErrorCode(errorCode: Int): Unit = {
    errorCode match {
      case CL_SUCCESS =>
      case _          => throw Exceptions.fromErrorCode(errorCode)
    }
  }

  trait UseFirstPlatform extends OpenCL {
    @transient
    protected lazy val platformId: PlatformId = {
      platformIds.head
    }
  }

  trait UseAllDevices extends OpenCL {

    @transient
    protected lazy val deviceIds: Seq[DeviceId] = {
      platformId.deviceIdsByType(CL_DEVICE_TYPE_ALL)
    }

  }

  trait UseFirstDevice extends OpenCL {

    @transient
    protected lazy val deviceIds: Seq[DeviceId] = {
      val allDeviceIds = platformId.deviceIdsByType(CL_DEVICE_TYPE_ALL)
      Seq(allDeviceIds.head)
    }

  }

  trait UseAllGpuDevices extends OpenCL {

    @transient
    protected lazy val deviceIds: Seq[DeviceId] = {
      platformId.deviceIdsByType(CL_DEVICE_TYPE_GPU)
    }
  }

  trait UseFirstGpuDevice extends OpenCL {

    @transient
    protected lazy val deviceIds: Seq[DeviceId] = {
      val allDeviceIds = platformId.deviceIdsByType(CL_DEVICE_TYPE_GPU)
      Seq(allDeviceIds.head)
    }
  }
  trait UseFirstCpuDevice extends OpenCL {

    @transient
    protected lazy val deviceIds: Seq[DeviceId] = {
      val allDeviceIds = platformId.deviceIdsByType(CL_DEVICE_TYPE_CPU)
      Seq(allDeviceIds.head)
    }
  }

  trait UseAllCpuDevices extends OpenCL {

    @transient
    protected lazy val deviceIds: Seq[DeviceId] = {
      platformId.deviceIdsByType(CL_DEVICE_TYPE_CPU)
    }
  }

  /**
    * @note [[HandleEventInExecutionContext]] __should__ be unnecessary because
    *       only OpenCL calls to create contexts or command-queues, or blocking OpenCL operations are undefined behavior,
    *       according to https://www.khronos.org/registry/OpenCL/sdk/1.2/docs/man/xhtml/clSetEventCallback.html
    *       and we don't use those forbidden functions.
    *       Our usage should be fine according to the OpenCL specification.
    *       However, AMD SDK always crashes for any reentry calls
    *       (e.g. https://travis-ci.org/Atry/DeepLearning.scala/jobs/318466522),
    *       no matter if they are blocking or not.
    *
    *       As a workaround, always enable this [[HandleEventInExecutionContext]] for AMD's OpenCL implementation.
    */
  trait HandleEventInExecutionContext extends OpenCL {
    val executionContext: ExecutionContext

    override protected def waitForStatus(event: Event, callbackType: Status): UnitContinuation[Status] =
      super.waitForStatus(event, callbackType).flatMap { status =>
        UnitContinuation.execute(status)(executionContext)
      }
  }

  trait GlobalExecutionContext {
    val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global
  }

  trait SingleThreadExecutionContext {
    val executionContext: ExecutionContextExecutor =
      scala.concurrent.ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
  }

  trait CommandQueuePool extends OpenCL {

    protected val numberOfCommandQueuesPerDevice: Int

    @transient private lazy val commandQueues: Seq[CommandQueue] = {
      deviceIds.flatMap { deviceId =>
        val capabilities = deviceCapabilities(deviceId)
        for (i <- 0 until numberOfCommandQueuesPerDevice) yield {
          val supportedProperties = deviceId.longInfo(CL_DEVICE_QUEUE_PROPERTIES)
          val properties = Map(
            CL_QUEUE_PROPERTIES -> (supportedProperties & CL_QUEUE_ON_DEVICE)
          )
          createCommandQueue(deviceId, properties)
        }
      }
    }

    @transient
    lazy val Resource(acquireCommandQueue, shutdownCommandQueues) = AsynchronousPool.preloaded(commandQueues)

    override def monadicClose: UnitContinuation[Unit] = {
      import scalaz.std.iterable._
      shutdownCommandQueues >> commandQueues.traverseU_(_.monadicClose) >> super.monadicClose
    }

  }

  final case class DeviceId[Owner <: Singleton with OpenCL](handle: Long) extends AnyVal {
    def deviceType: Long = {
      val stack = stackPush()
      try {
        val deviceTypeBuffer = stack.mallocLong(1)
        checkErrorCode(clGetDeviceInfo(handle, CL_DEVICE_TYPE, deviceTypeBuffer, null))
        deviceTypeBuffer.get(0)
      } finally {
        stack.close()
      }
    }

    def maxComputeUnits: Int = {
      val stack = stackPush()
      try {
        val outputBuffer = stack.mallocInt(1)
        checkErrorCode(clGetDeviceInfo(handle, CL_DEVICE_MAX_COMPUTE_UNITS, outputBuffer, null))
        outputBuffer.get(0)
      } finally {
        stack.close()
      }
    }

    def maxWorkGroupSize: Long = {
      val stack = stackPush()
      try {
        val bufferSizeBuffer = stack.mallocPointer(1)
        checkErrorCode(clGetDeviceInfo(handle, CL_DEVICE_MAX_WORK_GROUP_SIZE, bufferSizeBuffer, null))
        bufferSizeBuffer.get(0)
      } finally {
        stack.close()
      }
    }

    def maxWorkItemSizes: Seq[Long] = {
      val stack = stackPush()
      try {
        val bufferSizeBuffer = stack.mallocPointer(1)
        checkErrorCode(clGetDeviceInfo(handle, CL_DEVICE_MAX_WORK_ITEM_SIZES, null: PointerBuffer, bufferSizeBuffer))
        bufferSizeBuffer.get(0) match {
          case n if n > Int.MaxValue =>
            throw new IllegalStateException()
          case n =>
            val numberOfDimensions = n.toInt
            val sizeBuffer = stack.mallocPointer(numberOfDimensions)
            checkErrorCode(clGetDeviceInfo(handle, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeBuffer, null))
            val output = Array.ofDim[Long](numberOfDimensions)
            sizeBuffer.get(output)
            output
        }

      } finally {
        stack.close()
      }
    }

    private[OpenCL] def longInfo(paramName: Int) = {
      val buffer = Array[Long](0L)
      checkErrorCode(clGetDeviceInfo(handle, paramName, buffer, null))
      val Array(value) = buffer
      value
    }
  }

  object Event {
    private[OpenCL] val eventCallback: CLEventCallback = CLEventCallback.create(new CLEventCallbackI {
      final def invoke(event: Long, status: Int, userData: Long): Unit = {
        val scalaCallback = try { memGlobalRefToObject[Int => Unit](userData) } finally {
          JNINativeInterface.DeleteGlobalRef(userData)
        }
        scalaCallback(status)
      }
    })
    type Status = Int
  }

  final case class CommandQueue[Owner <: Singleton with OpenCL](handle: Long)
      extends AnyVal
      with MonadicCloseable[UnitContinuation] {

    def deviceId: DeviceId[Owner] = {
      val stack = stackPush()
      try {
        val deviceIdBuffer = stack.mallocPointer(1)
        checkErrorCode(clGetCommandQueueInfo(handle, CL_QUEUE_DEVICE, deviceIdBuffer, null))
        DeviceId(deviceIdBuffer.get(0))
      } finally {
        stack.close()
      }
    }

    def flush(): Unit = {
      checkErrorCode(clFlush(handle))
    }

    def monadicClose: UnitContinuation[Unit] = UnitContinuation.delay {
      checkErrorCode(clReleaseCommandQueue(handle))
    }
  }

  final case class Event[Owner <: Singleton with OpenCL](handle: Long)
      extends AnyVal
      with MonadicCloseable[UnitContinuation] {
    def release(): Unit = {
      checkErrorCode(clReleaseEvent(handle))
    }

    def retain(): Unit = {
      checkErrorCode(clRetainEvent(handle))
    }

    def referenceCount: Int = {
      val stack = stackPush()
      try {
        val intBuffer = stack.mallocInt(1)
        checkErrorCode(clGetEventInfo(handle, CL_EVENT_REFERENCE_COUNT, intBuffer, null))
        intBuffer.get(0)
      } finally {
        stack.close()
      }
    }

    def waitFor(callbackType: Status)(implicit
                                      witnessOwner: Witness.Aux[Owner]): Future[Unit] = {
      // The cast is a workaround for https://github.com/milessabin/shapeless/issues/753
      val self = this.asInstanceOf[witnessOwner.value.Event]

      val continuation = witnessOwner.value.waitForStatus(self, callbackType).flatMap[Try[Unit]] {
        case `callbackType` =>
          UnitContinuation.now(Success(()))
        case errorCode if errorCode < 0 =>
          UnitContinuation.now(Failure(Exceptions.fromErrorCode(errorCode)))
        case status =>
          throw new IllegalStateException(raw"""Invalid event status $status""")
      }
      Future(TryT(continuation))
    }

    def waitForComplete()(
        implicit
        witnessOwner: Witness.Aux[Owner]): Future[Unit] = waitFor(CL_COMPLETE)

    def monadicClose: UnitContinuation[Unit] = {
      UnitContinuation.delay {
        release()
      }
    }
  }

  object DeviceBuffer {

    implicit def bufferBox[Owner <: Singleton with OpenCL, Element]: Box.Aux[DeviceBuffer[Owner, Element], Pointer] =
      new Box[DeviceBuffer[Owner, Element]] {
        override type Raw = Pointer

        override def box(raw: Raw): DeviceBuffer[Owner, Element] =
          new DeviceBuffer[Owner, Element](raw.address())

        override def unbox(boxed: DeviceBuffer[Owner, Element]): Raw = new Pointer.Default(boxed.handle) {}
      }

  }

  /** A [[https://www.khronos.org/registry/OpenCL/sdk/2.1/docs/man/xhtml/abstractDataTypes.html cl_mem]]
    * whose [[org.lwjgl.opencl.CL10.CL_MEM_TYPE CL_MEM_TYPE]] is buffer [[org.lwjgl.opencl.CL10.CL_MEM_OBJECT_BUFFER CL_MEM_OBJECT_BUFFER]].
    * @param handle The underlying `cl_mem`.
    * @note comment out `extends AnyVal` in case of https://github.com/scala/bug/issues/10647
    */
  final case class DeviceBuffer[Owner <: OpenCL with Singleton, Element](handle: Long) /* extends AnyVal */
      extends MonadicCloseable[UnitContinuation] {
    deviceBuffer =>

    def monadicClose: UnitContinuation[Unit] = {
      UnitContinuation.delay {
        checkErrorCode(clReleaseMemObject(handle))
      }
    }

    def slice(offset: Int, size: Int)(implicit
                                      memory: Memory[Element]): Do[DeviceBuffer[Owner, Element]] = {

      Do.monadicCloseable {
        val stack = stackPush()
        try {
          val errorCode = stack.ints(0)
          val region = CLBufferRegion.mallocStack(stack)
          region.set(offset.toLong * memory.numberOfBytesPerElement, size.toLong * memory.numberOfBytesPerElement)
          val newHandle = nclCreateSubBuffer(handle,
                                             CL_MEM_READ_WRITE,
                                             CL_BUFFER_CREATE_TYPE_REGION,
                                             region.address(),
                                             memAddress(errorCode))
          checkErrorCode(errorCode.get(0))
          DeviceBuffer[Owner, Element](newHandle)
        } finally {
          stack.close()
        }
      }
    }

    def numberOfBytes: Int = {
      val sizeBuffer: Array[Long] = Array(0L)
      checkErrorCode(clGetMemObjectInfo(handle, CL_MEM_SIZE, sizeBuffer, null))
      val Array(value) = sizeBuffer
      if (value.isValidInt) {
        value.toInt
      } else {
        throw new IllegalStateException(s"Buffer's numberOfBytes($value) is too large")
      }
    }

    def length(implicit memory: Memory[Element]): Int = numberOfBytes / memory.numberOfBytesPerElement

    /** Returns an asynchronous operation of a buffer on host.
      *
      * The buffer may be [[java.nio.FloatBuffer FloatBuffer]],
      * [[java.nio.DoubleBuffer DoubleBuffer]]
      * or other buffer types according to `Element`.
      *
      * @note The buffer is allocated by lwjgl, not JRE.
      *       As a result, you can only use the buffer inside a `map` or `flatMap` block,
      *       then it will be released by [[com.thoughtworks.raii.asynchronous.Do Do]] automatically.
      *       Assigning the buffer to another variable used outside `map` or `flatMap` block
      *       will cause memory access error.
      *
      */
    final def toHostBuffer(preconditionEvents: Event[Owner]*)(implicit witnessOwner: Witness.Aux[Owner],
                                                              memory: Memory[Element]): Do[memory.HostBuffer] = {
      Do(TryT(ResourceT(UnitContinuation.delay {
        val hostBuffer = memory.allocate(length)
        Resource(value = Success(hostBuffer), release = UnitContinuation.delay { memory.free(hostBuffer) })
      }))).flatMap { hostBuffer =>
        witnessOwner.value
          .dispatchReadBuffer[Element, memory.HostBuffer](
            this.asInstanceOf[witnessOwner.value.DeviceBuffer[Element]],
            hostBuffer,
            preconditionEvents.asInstanceOf[Seq[witnessOwner.value.Event]]: _*)(memory)
          .intransitiveFlatMap { event =>
            Do.garbageCollected(event.waitForComplete()).map { _: Unit =>
              hostBuffer
            }
          }
      }
    }

  }

  final case class Kernel[Owner <: OpenCL with Singleton](handle: Long)
      extends AnyVal
      with MonadicCloseable[UnitContinuation] {

    def preferredWorkGroupSizeMultiple(deviceId: DeviceId[Owner]) = {
      val stack = stackPush()
      try {
        val pointerBuffer = stack.mallocPointer(1)
        checkErrorCode(
          clGetKernelWorkGroupInfo(handle,
                                   deviceId.handle,
                                   CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE,
                                   pointerBuffer,
                                   null))
        pointerBuffer.get(0)
      } finally {
        stack.close()
      }
    }

    def workGroupSize(deviceId: DeviceId[Owner]) = {
      val stack = stackPush()
      try {
        val pointerBuffer = stack.mallocPointer(1)
        checkErrorCode(
          clGetKernelWorkGroupInfo(handle, deviceId.handle, CL_KERNEL_WORK_GROUP_SIZE, pointerBuffer, null))
        pointerBuffer.get(0)
      } finally {
        stack.close()
      }
    }

    def setLocalMemorySize[A](argIndex: Int, size: Long)(implicit memory: Memory[A]): Unit = {
      checkErrorCode(nclSetKernelArg(handle, argIndex, size * memory.numberOfBytesPerElement, NULL))
    }

    def update[A](argIndex: Int, a: A)(implicit memory: Memory[A]): Unit = {
      val sizeofParameter = memory.numberOfBytesPerElement
      val stack = stackPush()
      try {
        val byteBuffer = stack.malloc(sizeofParameter)
        memory.put(memory.fromByteBuffer(byteBuffer), 0, a)
        checkErrorCode(nclSetKernelArg(handle, argIndex, sizeofParameter, memAddress(byteBuffer)))
      } finally {
        stack.close()
      }
    }

    def functionName: String = {
      val stack = stackPush()

      try {

        val functionNameSizePointer = stack.mallocPointer(1)

        checkErrorCode(
          clGetKernelInfo(this.handle, CL_KERNEL_FUNCTION_NAME, null: PointerBuffer, functionNameSizePointer))
        val functionNameSize = functionNameSizePointer.get(0).toInt
        val functionNameBuffer = stack.malloc(functionNameSize)

        checkErrorCode(
          clGetKernelInfo(this.handle, CL_KERNEL_FUNCTION_NAME, functionNameBuffer, functionNameSizePointer))
        decodeString(functionNameBuffer)
      } finally {
        stack.close()
      }
    }

    @inline
    def enqueue(commandQueue: CommandQueue[Owner],
                globalWorkOffset: Option[Seq[Long]] = None,
                globalWorkSize: Seq[Long],
                localWorkSize: Option[Seq[Long]] = None,
                waitingEvents: Seq[Long] = Array.empty[Long]): Do[Event[Owner]] = {

      def optionalStackPointerBuffer(option: Option[Seq[Long]]): MemoryStack => PointerBuffer = {
        option match {
          case None =>
            Function.const(null)
          case Some(pointers) =>
            _.pointers(pointers: _*)
        }
      }
      val globalWorkOffsetBuffer = optionalStackPointerBuffer(globalWorkOffset)
      val localWorkSizeBuffer = optionalStackPointerBuffer(localWorkSize)

      Do.monadicCloseable {
        val stack = stackPush()
        val outputEvent = try {
          val outputEventBuffer = stack.pointers(0L)
          val inputEventBuffer = if (waitingEvents.isEmpty) {
            null
          } else {
            stack.pointers(waitingEvents: _*)
          }
          checkErrorCode(
            clEnqueueNDRangeKernel(
              commandQueue.handle,
              handle,
              globalWorkSize.length,
              globalWorkOffsetBuffer(stack),
              stack.pointers(globalWorkSize: _*),
              localWorkSizeBuffer(stack),
              inputEventBuffer,
              outputEventBuffer
            )
          )
          Event[Owner](outputEventBuffer.get(0))
        } finally {
          stack.close()
        }
        commandQueue.flush()
        outputEvent
      }
    }

    def monadicClose: UnitContinuation[Unit] = {
      UnitContinuation.delay {
        checkErrorCode(clReleaseKernel(handle))
      }
    }
  }

  private[compute] final case class Program[Owner <: OpenCL with Singleton](handle: Long)
      extends AnyVal
      with MonadicCloseable[UnitContinuation] {

    private def numberOfKernels: Int = {
      val result = Array.ofDim[Int](1)
      checkErrorCode(clCreateKernelsInProgram(handle, null, result))
      result(0)
    }

    def deviceIds: Seq[DeviceId[Owner]] = {
      val stack = stackPush()
      try {
        val sizeBuffer = stack.mallocPointer(1)
        checkErrorCode(clGetProgramInfo(this.handle, CL_PROGRAM_DEVICES, null: PointerBuffer, sizeBuffer))
        val numberOfDeviceIds = sizeBuffer.get(0).toInt / POINTER_SIZE
        val programDevicesBuffer = stack.mallocPointer(numberOfDeviceIds)
        checkErrorCode(clGetProgramInfo(this.handle, CL_PROGRAM_DEVICES, programDevicesBuffer, sizeBuffer))
        (0 until numberOfDeviceIds).map { i =>
          DeviceId[Owner](programDevicesBuffer.get(i))
        }
      } finally {
        stack.close()
      }
    }

    def createKernels(): Seq[Kernel[Owner]] = {
      (0 until createKernelBuffer().capacity).map { i =>
        Kernel[Owner](createKernelBuffer().get(i))
      }
    }

    private def createKernelBuffer(): PointerBuffer = {
      val kernelBuffer = BufferUtils.createPointerBuffer(numberOfKernels)
      checkErrorCode(clCreateKernelsInProgram(handle, kernelBuffer, null: IntBuffer))
      kernelBuffer
    }

    def createFirstKernel(): Kernel[Owner] = {
      val stack = stackPush()
      try {
        val kernelBuffer = stack.mallocPointer(1)
        checkErrorCode(clCreateKernelsInProgram(handle, kernelBuffer, null: IntBuffer))
        Kernel(kernelBuffer.get(0))
      } finally {
        stack.close()
      }
    }

    private def buildLogs(deviceIds: Seq[DeviceId[Owner]]): Map[DeviceId[Owner], String] = {
      val stack = stackPush()
      try {
        val sizeBuffer = stack.mallocPointer(1)
        deviceIds.view.map { deviceId =>
          checkErrorCode(
            clGetProgramBuildInfo(this.handle, deviceId.handle, CL_PROGRAM_BUILD_LOG, null: PointerBuffer, sizeBuffer))
          val logBuffer = MemoryUtil.memAlloc(sizeBuffer.get(0).toInt) //stack.malloc()
          try {
            checkErrorCode(clGetProgramBuildInfo(this.handle, deviceId.handle, CL_PROGRAM_BUILD_LOG, logBuffer, null))
            (deviceId, decodeString(logBuffer))
          } finally {
            MemoryUtil.memFree(logBuffer)
          }
        }.toMap
      } finally {
        stack.close()
      }
    }

    private def checkBuildErrorCode(deviceIdsOption: Option[Seq[DeviceId[Owner]]], errorCode: Int): Unit = {
      errorCode match {
        case CL_BUILD_PROGRAM_FAILURE =>
          val logs = deviceIdsOption match {
            case None      => buildLogs(this.deviceIds)
            case Some(ids) => buildLogs(ids)
          }
          throw new Exceptions.BuildProgramFailure(logs)
        case _ => checkErrorCode(errorCode)
      }
    }

    def build(deviceIds: Seq[DeviceId[Owner]], options: CharSequence = ""): Unit = {
      val stack = stackPush()
      try {
        checkBuildErrorCode(
          Some(deviceIds),
          clBuildProgram(handle, stack.pointers(deviceIds.view.map(_.handle): _*), options, null, NULL))
      } finally {
        stack.close()
      }
    }

    def build(options: CharSequence): Unit = {
      checkBuildErrorCode(None, clBuildProgram(handle, null, options, null, NULL))
    }

    def build(): Unit = build("")

    def monadicClose = UnitContinuation.delay {
      OpenCL.checkErrorCode(clReleaseProgram(handle))
    }
  }

  object Program {

    @deprecated(
      message = "[[finalize]] method should not be invoked by users.",
      since = "[[finalize]] is deprecated in Java 9. However, it is the only way to clean up static native resources."
    )
    override protected def finalize(): Unit = {
      programCallback.close()
      super.finalize()
    }

    val programCallback = CLProgramCallback.create(new CLProgramCallbackI {
      def invoke(program: Long, userData: Long): Unit = {
        val scalaCallback = try {
          memGlobalRefToObject[Unit => Unit](userData)
        } finally {
          JNINativeInterface.DeleteGlobalRef(userData)
        }
        scalaCallback(())
      }
    })

  }

  private[OpenCL] object DontReleaseEventTooEarly {

    private[DontReleaseEventTooEarly] sealed trait EarlyEventState

    private[DontReleaseEventTooEarly] case object EarlyEventClosed extends EarlyEventState
    private[DontReleaseEventTooEarly] sealed trait EarlyEventList extends EarlyEventState
    private[DontReleaseEventTooEarly] case object EarlyEventNil extends EarlyEventList
    private[DontReleaseEventTooEarly] type AnyEvent = Event[_ <: Singleton with OpenCL]
    private[DontReleaseEventTooEarly] final case class EarlyEventCons(head: AnyEvent, tail: EarlyEventList)
        extends EarlyEventList
  }

  /** A plug-in that retains every [[Event]] created by `clEnqueueReadBuffer`
    * and waiting at least one second before releasing it.
    *
    * @note This is a workaround for https://github.com/ThoughtWorksInc/Compute.scala/issues/51
    */
  trait DontReleaseEventTooEarly extends OpenCL {
    import DontReleaseEventTooEarly._
    override protected def enqueueReadBuffer[Element, Destination](
        commandQueue: CommandQueue,
        deviceBuffer: DeviceBuffer[Element],
        hostBuffer: Destination,
        preconditionEvents: Event*)(implicit memory: Memory.Aux[Element, Destination]): Do[Event] =
      super.enqueueReadBuffer(commandQueue, deviceBuffer, hostBuffer, preconditionEvents: _*).map { event =>
        @tailrec
        def enqueueEvent(): Unit = {
          newlyCreatedEvents.get() match {
            case oldState: EarlyEventList =>
              if (newlyCreatedEvents.compareAndSet(oldState, EarlyEventCons(event, oldState))) {
                event.retain()
              } else {
                enqueueEvent()
              }
            case EarlyEventClosed =>
              throw new IllegalStateException()
          }
        }
        enqueueEvent()
        event
      }

    // TODO: specialize the List[Event]
    private val newlyCreatedEvents = new AtomicReference[EarlyEventState](EarlyEventNil)

    @volatile
    private var shuttingDownHandler: Option[Unit => Unit] = None
    private val earlyEventThread = new Thread {
      setDaemon(true)

      override def run(): Unit = {

        @tailrec
        def releaseAll(state: EarlyEventState): Unit = {
          state match {
            case EarlyEventNil =>
            case EarlyEventCons(head, tail) =>
              head.release()
              releaseAll(tail)
            case EarlyEventClosed =>
              throw new IllegalStateException()
          }
        }
        Thread.sleep(1000L)
        while (shuttingDownHandler.isEmpty) {
          val state = newlyCreatedEvents.getAndSet(EarlyEventNil)
          Thread.sleep(1000L)

          releaseAll(state)
        }
        val state = newlyCreatedEvents.getAndSet(EarlyEventClosed)
        releaseAll(state)
        shuttingDownHandler.get(())
      }
    }

    earlyEventThread.start()

    override def monadicClose: UnitContinuation[Unit] = {
      UnitContinuation.async[Unit] { continue =>
        shuttingDownHandler = Some(continue)
      } >> super.monadicClose
    }
  }

  private implicit object CanLogPrivateInfo extends CanLog[ByteBuffer] {

    private def toHexString(buffer: ByteBuffer): String = {
      if (buffer.remaining > 0) {
        val hexText = for (i <- (buffer.position() until buffer.limit).view) yield {
          f"${buffer.get(i)}%02X"
        }
        hexText.mkString(" ")
      } else {
        ""
      }
    }

    private final val mdcKey = "pfn_notify.private_info"

    def logMessage(originalMessage: String, privateInfo: ByteBuffer): String = {
      MDC.put(mdcKey, toHexString(privateInfo))
      originalMessage
    }

    override def afterLog(privateInfo: ByteBuffer): Unit = {
      MDC.remove(mdcKey)
      super.afterLog(privateInfo)
    }
  }

  trait LogContextNotification extends OpenCL {

    protected def handleOpenCLNotification(errorInfo: String, privateInfoOption: Option[ByteBuffer]): Unit = {
      privateInfoOption match {
        case None =>
          logger.info(errorInfo)
        case Some(privateInfo) =>
          Logger.takingImplicit[ByteBuffer](logger.underlying).info(errorInfo)(privateInfo)
      }
    }
  }

}

trait OpenCL extends MonadicCloseable[UnitContinuation] with ImplicitsSingleton with DefaultCloseable {
  import OpenCL._

  protected val logger: Logger

  type Program = OpenCL.Program[this.type]
  type Event = OpenCL.Event[this.type]
  type CommandQueue = OpenCL.CommandQueue[this.type]
  type DeviceId = OpenCL.DeviceId[this.type]
  type PlatformId = OpenCL.PlatformId[this.type]

  def platformIds: Seq[PlatformId] = {
    val stack = stackPush()
    try {
      val numberOfPlatformsBuffer = stack.mallocInt(1)
      checkErrorCode(clGetPlatformIDs(null, numberOfPlatformsBuffer))
      val numberOfPlatforms = numberOfPlatformsBuffer.get(0)
      val platformIdBuffer = stack.mallocPointer(numberOfPlatforms)
      checkErrorCode(clGetPlatformIDs(platformIdBuffer, null: IntBuffer))
      (0 until numberOfPlatforms).map { i =>
        new PlatformId(platformIdBuffer.get(i))
      }
    } finally {
      stack.close()
    }
  }

  private[OpenCL] def dispatchReadBuffer[Element, Destination](
      deviceBuffer: DeviceBuffer[Element],
      hostBuffer: Destination,
      preconditionEvents: Event*)(implicit memory: Memory.Aux[Element, Destination]): Do[Event] = {
    dispatch(enqueueReadBuffer(_, deviceBuffer, hostBuffer, preconditionEvents: _*))
  }

  protected def enqueueReadBuffer[Element, Destination](commandQueue: CommandQueue,
                                                        deviceBuffer: DeviceBuffer[Element],
                                                        hostBuffer: Destination,
                                                        preconditionEvents: Event*)(
      implicit
      memory: Memory.Aux[Element, Destination]): Do[Event] = {
    Do.monadicCloseable {
      val outputEvent = {
        val stack = stackPush()
        try {
          val (inputEventBufferSize, inputEventBufferAddress) = if (preconditionEvents.isEmpty) {
            (0, NULL)
          } else {
            val inputEventBuffer = stack.pointers(preconditionEvents.view.map(_.handle): _*)
            (preconditionEvents.length, inputEventBuffer.address())
          }
          val outputEventBuffer = stack.pointers(0L)
          checkErrorCode(
            nclEnqueueReadBuffer(
              commandQueue.handle,
              deviceBuffer.handle,
              CL_FALSE,
              0,
              memory.remainingBytes(hostBuffer),
              memory.address(hostBuffer),
              inputEventBufferSize,
              inputEventBufferAddress,
              outputEventBuffer.address()
            )
          )
          new Event(outputEventBuffer.get(0))
        } finally {
          stack.close()
        }
      }
      commandQueue.flush()
      outputEvent
    }
  }

  protected def waitForStatus(event: Event, callbackType: Status): UnitContinuation[Status] = UnitContinuation.async {
    (continue: Status => Unit) =>
      val userData = JNINativeInterface.NewGlobalRef(continue)
      try {
        checkErrorCode(
          clSetEventCallback(
            event.handle,
            callbackType,
            Event.eventCallback,
            userData
          )
        )
      } catch {
        case NonFatal(e) =>
          //JNINativeInterface.DeleteGlobalRef(userData)
          throw e
      }
  }

  type Kernel = OpenCL.Kernel[this.type]
  protected def createProgramWithSource(sourceCode: TraversableOnce[CharSequence]): Program = {
    val stack = stackPush()
    try {
      val errorCodeBuffer = stack.ints(CL_SUCCESS)
      val codeBuffers = (for {
        snippet <- sourceCode
        if snippet.length > 0
      } yield memUTF8(snippet, false)).toArray
      val pointers = memAllocPointer(codeBuffers.length)
      val lengths = memAllocPointer(codeBuffers.length)
      for (buffer <- codeBuffers) {
        pointers.put(buffer)
        lengths.put(buffer.remaining)
      }
      pointers.position(0)
      lengths.position(0)
      try {
        val programHandle = clCreateProgramWithSource(context, pointers, lengths, errorCodeBuffer)
        checkErrorCode(errorCodeBuffer.get(0))
        new Program(programHandle)
      } finally {
        memFree(pointers)
        memFree(lengths)
        codeBuffers.foreach(memFree)
      }
    } finally {
      stack.close()
    }
  }

  protected def acquireCommandQueue: Do[CommandQueue]

  protected def dispatch(command: CommandQueue => Do[Event]): Do[Event] = {

    val Do(TryT(ResourceT(acquireContinuation))) = acquireCommandQueue

    Do.garbageCollected(acquireContinuation).flatMap {
      case Resource(Success(commandQueue), release) =>
        (try command(commandQueue)
        catch {
          case NonFatal(e) =>
            e.raiseError[Do, Event]
        }).handleError { e =>
            release.onComplete(identity)
            e.raiseError[Do, Event]
          }
          .map { event =>
            event
              .waitForComplete()
              .onComplete { result =>
                release.onComplete(identity)
                result match {
                  case Success(()) =>
                  case Failure(e) =>
                    logger.error(s"Cannot wait for cl_event[handle = ${event.handle}]", e)
                }
              }
            event
          }
      case r @ Resource(Failure(e), release) =>
        Do(TryT(ResourceT(UnitContinuation.now(r.asInstanceOf[Resource[UnitContinuation, Try[Event]]]))))
    }

  }

  protected def releaseContext: UnitContinuation[Unit] = UnitContinuation.delay {
    checkErrorCode(clReleaseContext(context))
  }

  override def monadicClose: UnitContinuation[Unit] = {
    releaseContext >> super.monadicClose
  }

  protected def handleOpenCLNotification(errorInfo: String, privateInfo: Option[ByteBuffer]): Unit

  import OpenCL._

  protected val platformId: PlatformId
  protected val deviceIds: Seq[DeviceId]

  @transient
  protected lazy val platformCapabilities: CLCapabilities = {
    CL.createPlatformCapabilities(platformId.handle)
  }

  protected def createCommandQueue(deviceId: DeviceId, properties: Map[Int, Long]): CommandQueue = new CommandQueue(
    if (deviceCapabilities(deviceId).OpenCL20) {
      val cl20Properties = (properties.view.flatMap { case (key, value) => Seq(key, value) } ++ Seq(0L)).toArray
      val a = Array(0)
      val commandQueue =
        clCreateCommandQueueWithProperties(platformId.handle, deviceId.handle, cl20Properties, a)
      checkErrorCode(a(0))
      commandQueue
    } else {
      val cl10Properties = properties.getOrElse(CL_QUEUE_PROPERTIES, 0L)
      val a = Array(0)
      val commandQueue = clCreateCommandQueue(context, deviceId.handle, cl10Properties, a)
      checkErrorCode(a(0))
      commandQueue
    }
  )

  @transient
  protected lazy val deviceCapabilities: DeviceId => CLCapabilities = {
    Memo.mutableMapMemo(new ConcurrentHashMap[DeviceId, CLCapabilities].asScala) { deviceId =>
      CL.createDeviceCapabilities(deviceId.handle, platformCapabilities)
    }
  }

  @transient
  protected lazy val context: Long = {
    val stack = stackPush()
    try {
      val errorCodeBuffer = stack.ints(CL_SUCCESS)
      val contextProperties = stack.pointers(CL_CONTEXT_PLATFORM, platformId.handle, 0)
      val deviceIdBuffer = stack.pointers(deviceIds.view.map(_.handle): _*)
      val context =
        clCreateContext(contextProperties,
                        deviceIdBuffer,
                        OpenCL.contextCallback,
                        JNINativeInterface.NewWeakGlobalRef(this),
                        errorCodeBuffer)
      checkErrorCode(errorCodeBuffer.get(0))
      context
    } finally {
      stack.close()
    }
  }
  trait ImplicitsApi {}
  type Implicits <: ImplicitsApi

  val implicits: Implicits

  type DeviceBuffer[Element] = OpenCL.DeviceBuffer[this.type, Element]

  /** Returns an uninitialized buffer of `Element` on device.
    */
  def allocateBuffer[Element](size: Long)(implicit memory: Memory[Element]): Do[DeviceBuffer[Element]] =
    Do.monadicCloseable {
      val stack = stackPush()
      try {
        val errorCodeBuffer = stack.ints(CL_SUCCESS)
        val buffer =
          clCreateBuffer(context, CL_MEM_READ_WRITE, memory.numberOfBytesPerElement * size, errorCodeBuffer)
        checkErrorCode(errorCodeBuffer.get(0))
        DeviceBuffer[this.type, Element](buffer)
      } finally {
        stack.pop()
      }
    }

  /** Returns a buffer of `Element` on device whose content is copied from `hostBuffer`.
    */
  def allocateBufferFrom[Element, HostBuffer](hostBuffer: HostBuffer)(
      implicit memory: Memory.Aux[Element, HostBuffer]): Do[DeviceBuffer[Element]] =
    Do.monadicCloseable {
      val stack = stackPush()
      try {
        val errorCodeBuffer = stack.ints(CL_SUCCESS)
        val buffer = nclCreateBuffer(context,
                                     CL_MEM_COPY_HOST_PTR | CL_MEM_READ_WRITE,
                                     memory.remainingBytes(hostBuffer),
                                     memory.address(hostBuffer),
                                     memAddress(errorCodeBuffer))
        checkErrorCode(errorCodeBuffer.get(0))
        DeviceBuffer[this.type, Element](buffer)
      } finally {
        stack.pop()
      }
    }

}
