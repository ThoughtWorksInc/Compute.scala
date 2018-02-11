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
  private def doTensors: Do[Tensors] =
    Do.monadicCloseable(Factory[
      OpenCL.GlobalExecutionContext with OpenCL.UseAllDevices with OpenCL.UseFirstPlatform with OpenCL.CommandQueuePool with Tensors with OpenCL.DontReleaseEventTooEarly]
      .newInstance(
        handleOpenCLNotification = handleOpenCLNotification,
        numberOfCommandQueuesForDevice = { (deviceId: Long, capabilities: CLCapabilities) =>
          5
        }
      ))

  "repeatedly toString" in {
    doTensors.map { tensors =>
      val tensor = tensors.Tensor(42.0f)
      for (i <- 0 until 1000) {
        tensor.toString should be("42.0")
      }
      succeed
    }
  }.run.toScalaFuture

  "create a tensor of a constant" in {
    doTensors.flatMap { tensors =>
      val shape = Array(2, 3, 5)
      val element = 42.0f
      val filled = tensors.Tensor.fill(element, shape)
      for {
        pendingBuffer <- filled.enqueue
        floatBuffer <- pendingBuffer.toHostBuffer
      } yield {
        for (i <- 0 until floatBuffer.capacity()) {
          floatBuffer.get(i) should be(element)
        }
        floatBuffer.position() should be(0)
        floatBuffer.limit() should be(shape.product)
        floatBuffer.capacity() should be(shape.product)
        tensors.kernelCache.getIfPresent(filled.closure) should not be null
        val zeros2 = tensors.Tensor.fill(element, shape)
        tensors.kernelCache.getIfPresent(zeros2.closure) should not be null
      }
    }
  }.run.toScalaFuture

  "tensor literal" in {
    doTensors.map { tensors =>
      tensors.Tensor(42.0f).toString should be("42.0")

      tensors.Tensor(Array(1.0f, 2.0f)).toString should be("[1.0,2.0]")

      tensors.Tensor(Array(Seq(1.0f, 2.0f), List(3.0f, 4.0f))).toString should be("[[1.0,2.0],[3.0,4.0]]")
    }
  }.run.toScalaFuture

  "Wrong tensor shape" in {
    doTensors.map { tensors =>
      an[IllegalArgumentException] should be thrownBy {
        tensors.Tensor(Seq(Array(1.0f), Array(3.0f, 4.0f)))
      }
    }
  }.run.toScalaFuture

  "translate" in {
    doTensors.flatMap { tensors =>
      val shape = Array(2, 3, 5)
      val element = 42.0f
      val padding = 99.0f
      val translated = tensors.Tensor.fill(element, shape, padding = padding).translate(Array(1, 2, -3))
      translated.toString should be(
        "[" +
          "[" +
          "[99.0,99.0,99.0,99.0,99.0]," +
          "[99.0,99.0,99.0,99.0,99.0]," +
          "[99.0,99.0,99.0,99.0,99.0]" +
          "]," +
          "[" +
          "[99.0,99.0,99.0,99.0,99.0]," +
          "[99.0,99.0,99.0,99.0,99.0]," +
          "[42.0,42.0,99.0,99.0,99.0]" +
          "]" +
          "]")

      for {
        pendingBuffer <- translated.enqueue
        floatBuffer <- pendingBuffer.toHostBuffer
      } yield {
        floatBuffer.position() should be(0)
        val array = Array.ofDim[Float](shape.product)
        floatBuffer.get(array)
        val array3d = array.grouped(shape(2)).grouped(shape(1))
        for ((xi, i) <- array3d.zipWithIndex; (xij, j) <- xi.zipWithIndex; (xijk, k) <- xij.view.zipWithIndex) {
          if (i >= 1 && j >= 2 && 5 - k > 3) {
            xijk should be(element)
          } else {
            xijk should be(padding)
          }
        }
        floatBuffer.limit() should be(shape.product)
        floatBuffer.capacity() should be(shape.product)
      }
    }
  }.run.toScalaFuture

  "convolution" ignore {
    doTensors.flatMap { tensors =>
      import tensors.Tensor
      import tensors.zip
      def convolute(input: Tensor /* batchSize × height × width × depth */,
                    weight: Tensor /* kernelHeight × kernelWidth × depth × filterSize */,
                    bias: Tensor /* filterSize*/ ): Tensor = {
        input.shape match {
          case Array(batchSize, height, width, depth) =>
            weight.shape match {
              case Array(kernelHeight, kernelWidth, `depth`, filterSize) =>
                bias.shape match {
                  case Array(`filterSize`) =>
                    val inputSeq: Seq[Tensor /* batchSize × height × width */ ] = input.unzip(dimension = 3)

                    val weightSeq: Seq[Seq[Seq[Seq[Tensor]]]] /* filterSize × kernelHeight × kernelWidth × depth */ =
                      weight
                        .unzip(dimension = 3)
                        .map(_.unzip(dimension = 0).map(_.unzip(dimension = 0).map(_.unzip(dimension = 0))))

                    val biasSeq: Seq[Tensor] /* filterSize */ = bias.unzip(dimension = 0)

                    val outputChannels: Seq[Tensor] = weightSeq.view.zip(biasSeq).map {
                      case (weightPerFilter, biasPerFilter) =>
                        val summands: Seq[Tensor] = for {
                          (offsetY, weightPerRow) <- (-1 to 1).view.zip(weightPerFilter)
                          (offsetX, weightPerPixel) <- (-1 to 1).view.zip(weightPerRow)
                          (
                            inputPerChannel /* batchSize × height × width */,
                            weightPerChannel /* scalar */
                          ) <- inputSeq.view.zip(weightPerPixel)
                        } yield {
                          inputPerChannel.translate(Array(0, offsetY, offsetX)) *
                            weightPerChannel.broadcast(Array(batchSize, height, width))
                        }

                        biasPerFilter.broadcast(Array(batchSize, height, width)) + summands.reduce(_ + _)
                    }
                    zip(outputChannels, dimension = 3)
                  case _ =>
                    throw new IllegalArgumentException
                }
              case _ =>
                throw new IllegalArgumentException
            }
          case _ =>
            throw new IllegalArgumentException
        }
      }

      ??? : Do[Assertion]
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
