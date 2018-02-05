package com.thoughtworks.compute

import java.nio.{ByteBuffer, FloatBuffer}

import com.thoughtworks.feature.Factory
import TensorsSpec._
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import org.lwjgl.BufferUtils
import org.lwjgl.opencl.CLCapabilities
import org.lwjgl.system.MemoryUtil

import scalaz.syntax.all._
import scala.language.existentials
import org.scalatest._

import scalaz.EphemeralStream

/**
  * @author 杨博 (Yang Bo)
  */
class TensorsSpec extends AsyncFreeSpec with Matchers {
  private def doTensors: Do[Tensors] =
    Do.monadicCloseable(Factory[
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

  "translate" in {
    import scalaz.std.anyVal._

    ((0 |=> 10): EphemeralStream[Int]).traverseU_ { i =>

      locally {
        doTensors.flatMap { tensors =>
          val shape = Array(2, 3, 5)
          val element = 42.0f
          val padding = 99.0f
          val translated = tensors.Tensor.fill(element, shape, padding = padding).translate(Array(1, 2, -3))
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
      }.run
    }.toScalaFuture.map{_:Unit =>
      succeed
    }
  }

  "convolution" ignore {
    doTensors.flatMap { tensors =>
      import tensors.Tensor
      import tensors.concatenate
      def convolute(input: Tensor /* batchSize × height × width × depth */,
                    weight: Tensor /* kernelHeight × kernelWidth × depth × filterSize */,
                    bias: Tensor /* filterSize*/ ): Tensor = {
        input.shape match {
          case Array(batchSize, height, width, depth) =>
            weight.shape match {
              case Array(kernelHeight, kernelWidth, `depth`, filterSize) =>
                bias.shape match {
                  case Array(`filterSize`) =>
                    val inputSeq: Seq[Tensor /* batchSize × height × width */ ] = input.split(dimension = 3)

                    val weightSeq: Seq[Seq[Seq[Seq[Tensor]]]] /* filterSize × kernelHeight × kernelWidth × depth */ =
                      weight
                        .split(dimension = 3)
                        .map(_.split(dimension = 0).map(_.split(dimension = 0).map(_.split(dimension = 0))))

                    val biasSeq: Seq[Tensor] /* filterSize */ = bias.split(dimension = 0)

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
                    concatenate(outputChannels, dimension = 3)
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
