package com.thoughtworks.compute

import java.nio.{ByteBuffer, FloatBuffer}

import com.thoughtworks.feature.Factory
import TensorsSpec._
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import org.lwjgl.opencl.CLCapabilities

import scalaz.syntax.all._
import scala.language.existentials
import org.scalatest._

/**
  * @author 杨博 (Yang Bo)
  */
class TensorsSpec extends AsyncFreeSpec with Matchers {
  private def doTensors: Do[Tensors] = Do.monadicCloseable(
    Factory[
      OpenCL.GlobalExecutionContext with OpenCL.UseAllDevices with OpenCL.UseFirstPlatform with OpenCL.CommandQueuePool with Tensors]
      .newInstance(
        handleOpenCLNotification = handleOpenCLNotification,
        numberOfCommandQueuesForDevice = { (deviceId: Long, capabilities: CLCapabilities) =>
          5
        }
      ))

  "create a tensor of a constant" in {
    doTensors.flatMap { tensors =>
      val shape = Array(2, 3, 5)
      val element = 42.0f
      val zeros = tensors.Tensor.fill(element, shape)
      for {
        pendingBuffer <- zeros.enqueue
        floatBuffer <- pendingBuffer.toHostBuffer
      } yield {
        for (i <- 0 until floatBuffer.capacity()) {
          floatBuffer.get(i) should be(element)
        }
        floatBuffer.position() should be(0)
        floatBuffer.limit() should be(shape.product)
        floatBuffer.capacity() should be(shape.product)
        tensors.kernelCache.getIfPresent(zeros.closure) should not be null
        val zeros2 = tensors.Tensor.fill(element, shape)
        tensors.kernelCache.getIfPresent(zeros2.closure) should not be null
      }
    }
  }.run.toScalaFuture

}

object TensorsSpec {

  private val handleOpenCLNotification = { (errorInfo: String, buffer: ByteBuffer) =>
    if (buffer.remaining > 0) {
      val hexText = for (i <- (buffer.position() until buffer.limit).view) yield {
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
