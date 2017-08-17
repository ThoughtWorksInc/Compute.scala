package com.thoughtworks.compute

import java.nio.ByteBuffer
import java.util.concurrent.ForkJoinPool

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.continuation._
import com.thoughtworks.feature.{Factory, ImplicitApply}
import com.thoughtworks.feature.mixins.ImplicitsSingleton
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import org.lwjgl.opencl.CL10._
import org.lwjgl.opencl.CLCapabilities
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._
import org.openjdk.jmh.annotations.{Benchmark, Param, Scope, State}
import shapeless.Witness

import scala.concurrent.ExecutionContext
import scalaz.Semigroup
import scalaz.std.stream._
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.Tags.Parallel

/**
  * @author 杨博 (Yang Bo)
  */
object OpenCLBenchmark {
  implicit val executionContext = ExecutionContext.fromExecutorService(new ForkJoinPool(50))

  trait TestKernels extends OpenCL with OpenCL.CommandQueuePool {

    @transient
    private[OpenCLBenchmark] lazy val compiledProgram: Program = {

      val program = createProgramWithSource(fastraw"""
      float sample(global const float* restrict input, const size_t image_index, const ptrdiff_t x, const ptrdiff_t y, const ptrdiff_t width, const ptrdiff_t height) {
        if (x >= 0 && x < width && y >= 0 && y < height) {
          return input[image_index * width * height, y * width + x];
        } else {
          return 0.0f;
        }
      }

      kernel void benchmark(global const float* restrict input, global float* restrict output, global const float* restrict weight) {
        const size_t image_index = get_global_id(0);
        const size_t batch_size = get_global_size(0);
        const size_t x = get_global_id(1);
        const size_t width = get_global_size(1);
        const size_t y = get_global_id(2);
        const size_t height = get_global_size(2);
        output[image_index * width * height + y * width + x] = ${(for {
        offsetX <- ConvolutionalKernelX
        offsetY <- ConvolutionalKernelY
      } yield fast"sample(input, image_index, x + ($offsetX), y + ($offsetY), width, height)").mkFastring(" + ")};
      }
      """)

      program.build()
      program

    }

    override def monadicClose: UnitContinuation[Unit] = {
      compiledProgram.monadicClose >> super.monadicClose
    }

    def test(input: DeviceBuffer[Float],
             output: DeviceBuffer[Float],
             weight: DeviceBuffer[Float],
             batchSize: Int,
             width: Int,
             height: Int): Future[Unit] = {
      Do.monadicCloseable(compiledProgram.firstKernel)
        .flatMap { kernel =>
          kernel(0) = input
          kernel(1) = output
          kernel(2) = weight
          val self: this.type = this
          kernel.enqueue(batchSize, width, height)(Witness(self)).flatMap { event =>
            Do.garbageCollected(event.waitForComplete())
          }
        }
        .run

    }
  }

  final val ConvolutionalKernelX = -1 to 1
  final val ConvolutionalKernelY = -1 to 1
  final val ConvolutionalKernelSize: Int = ConvolutionalKernelX.length * ConvolutionalKernelY.length

  private val handleOpenCLNotification = { (errorInfo: String, buffer: ByteBuffer) =>
    if (buffer.remaining > 0) {
      val hexText = for (i <- (buffer.position until buffer.limit).view) yield {
        f"${buffer.get(i)}%02X"
      }
      Console.err.println(hexText.mkString(errorInfo, " ", ""))
      Console.err.flush()
    } else {
      Console.err.println(errorInfo)
      Console.err.flush()
    }
  }
}

@State(Scope.Benchmark)
class OpenCLBenchmark {
  import OpenCLBenchmark._

  @Param(Array("1", "32", "64", "8"))
  var width: Int = 32
  @Param(Array("1", "32", "64", "8"))
  var height: Int = 32

  @Param(Array("1", "8", "128", "32"))
  var batchSize: Int = 128

  @Param(Array("1", "32", "8"))
  var numberOfConcurrentLayers: Int = 1

  @Param(Array("1", "1024", "256"))
  var totalLayers: Int = 1024

  @Benchmark
  def testTheNet(): Unit = {

    val numberOfCommandQueuesForDevice = { (deviceId: Long, capabilities: CLCapabilities) =>
      numberOfConcurrentLayers
    }
    val doOpenCL = Do.monadicCloseable {
      Factory[
        TestKernels with OpenCL with OpenCL.UseAllDevices with OpenCL.UseFirstPlatform with ImplicitsSingleton with OpenCL.CommandQueuePool]
        .newInstance(
          handleOpenCLNotification = handleOpenCLNotification,
          numberOfCommandQueuesForDevice = numberOfCommandQueuesForDevice
        )
    }

    doOpenCL
      .flatMap { opencl2 =>
        val opencl = opencl2 // Workaround for https://github.com/milessabin/shapeless/issues/749
        import opencl.implicits._

        val sliceSize = width * height * batchSize

        for {
//          inputDeviceBuffer <- opencl.allocateBuffer[Float](sliceSize * totalLayers)
//          inputSlices <- (0 until totalLayers).toStream.traverse { i =>
//            inputDeviceBuffer.slice(sliceSize * i, sliceSize)
//          }
//          outputDeviceBuffer <- opencl.allocateBuffer[Float](sliceSize * totalLayers)
//          outputSlices <- (0 until totalLayers).toStream.traverse { i =>
//            outputDeviceBuffer.slice(sliceSize * i, sliceSize)
//          }
//          weightDeviceBuffer <- opencl.allocateBuffer[Float](ConvolutionalKernelSize * totalLayers)
//          weightSlices <- (0 until totalLayers).toStream.traverse { i =>
//            weightDeviceBuffer.slice(ConvolutionalKernelSize * i, ConvolutionalKernelSize)
//          }

          //          inputDeviceBuffer <- opencl.allocateBuffer[Float](sliceSize * totalLayers)
          inputSlices <- (0 until totalLayers).toStream.traverse { i =>
            opencl.allocateBuffer[Float](sliceSize)
          }
          //          outputDeviceBuffer <- opencl.allocateBuffer[Float](sliceSize * totalLayers)
          outputSlices <- (0 until totalLayers).toStream.traverse { i =>
            opencl.allocateBuffer[Float](sliceSize)
          }
          //          weightDeviceBuffer <- opencl.allocateBuffer[Float](ConvolutionalKernelSize * totalLayers)
          weightSlices <- (0 until totalLayers).toStream.traverse { i =>
            opencl.allocateBuffer[Float](ConvolutionalKernelSize)
          }

          unit <- Do.garbageCollected {
            implicit def keepLastException = new Semigroup[Throwable] {
              override def append(f1: Throwable, f2: => Throwable) = f2
            }
            (0 until totalLayers).toStream
              .traverse_[ParallelFuture] { layerIndex =>
                Parallel(
                  Future.execute(()) >>
                    opencl.test(
                      inputSlices(layerIndex),
                      outputSlices(layerIndex),
                      weightSlices(layerIndex),
                      batchSize,
                      width,
                      height
                    ))
              }
              .unwrap
          }
        } yield unit

      }
      .run
      .blockingAwait

  }

}
