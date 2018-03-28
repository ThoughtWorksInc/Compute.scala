package com.thoughtworks.compute

import java.nio.{ByteBuffer, FloatBuffer}

import com.thoughtworks.compute.OpenCL.Exceptions
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.raii.asynchronous._
import com.typesafe.scalalogging.StrictLogging
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
      Tensors.WangHashingRandomNumberGenerator with StrictLogging with OpenCL.LogContextNotification with OpenCL.GlobalExecutionContext with OpenCL.UseAllDevices with OpenCL.CommandQueuePool with Tensors with OpenCL.DontReleaseEventTooEarly]
      .newInstance(
        numberOfCommandQueuesPerDevice = 5
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
        pendingBuffer <- filled.doBuffer
        floatBuffer <- pendingBuffer.toHostBuffer
      } yield {
        for (i <- 0 until floatBuffer.capacity()) {
          floatBuffer.get(i) should be(element)
        }
        floatBuffer.position() should be(0)
        floatBuffer.limit() should be(shape.product)
        floatBuffer.capacity() should be(shape.product)
        tensors.kernelCache.getIfPresent(filled.getClosure) should not be null
        val zeros2 = tensors.Tensor.fill(element, shape)
        tensors.kernelCache.getIfPresent(zeros2.getClosure) should not be null
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
        pendingBuffer <- translated.doBuffer
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

  "unzip" in {
    doTensors.map { tensors =>
      import tensors._
      val tensor = Tensor(Seq(Seq(Seq(Seq(1.0f, 5.0f)))))
      tensor.unzip(dimension = 3).map(_.toString) should be(Seq("[[[1.0]]]", "[[[5.0]]]"))
    }
  }.run.toScalaFuture

  "plus" in {
    doTensors.map { tensors =>
      import tensors._
      val tensor = Tensor(Seq(Seq(Seq(1.0f, 5.0f))))
      (tensor + tensor).toString should be("[[[2.0,10.0]]]")
    }
  }.run.toScalaFuture

  "plus and multiplication" in {
    doTensors.map { tensors =>
      import tensors._
      val tensor = Tensor(Seq(Seq(Seq(1.0f, 5.0f))))
      val tensor2 = tensor + tensor
      (tensor2 * tensor2).toString should be("[[[4.0,100.0]]]")
    }
  }.run.toScalaFuture

  "convolution" in {
    doTensors.flatMap { tensors =>
      import tensors.Tensor
      import tensors.Tensor.zip
      def convolute(input: Tensor /* batchSize × height × width × depth */,
                    weight: Tensor /* kernelHeight × kernelWidth × depth × filterSize */,
                    bias: Tensor /* filterSize */ ): Tensor = {
        input.shape match {
          case Array(batchSize, height, width, depth) =>
            weight.shape match {
              case Array(kernelHeight, kernelWidth, `depth`, filterSize) =>
                bias.shape match {
                  case Array(`filterSize`) =>
                    val inputSeq: Seq[Tensor /* batchSize × height × width */ ] = input.unzip(dimension = 3)

                    inputSeq.size should be(depth)
                    inputSeq.head.shape should be(Array(batchSize, height, width))

                    val weightSeq: Seq[Seq[Seq[Seq[Tensor]]]] /* filterSize × kernelHeight × kernelWidth × depth */ =
                      weight.unzip(dimension = 3).map { khKwD =>
                        khKwD.shape should be(Array(kernelHeight, kernelWidth, depth))

                        khKwD.unzip(dimension = 0).map { kwD =>
                          kwD.shape should be(Array(kernelWidth, depth))
                          kwD.unzip(dimension = 0).map { d =>
                            d.shape should be(Array(depth))
                            d.unzip(dimension = 0)
                          }
                        }
                      }

                    weightSeq.length should be(filterSize)
                    weightSeq.head.length should be(kernelHeight)
                    weightSeq.head.head.length should be(kernelWidth)
                    weightSeq.head.head.head.length should be(depth)

                    val biasSeq: Seq[Tensor] /* filterSize */ = bias.unzip(dimension = 0)

                    val outputChannels: Seq[Tensor] = weightSeq.view
                      .zip(biasSeq)
                      .map {
                        case (weightPerFilter, biasPerFilter) =>
                          val summands: Seq[Tensor] = for {
                            (offsetY, weightPerRow) <- (-1 to 1).view.zip(weightPerFilter)
                            (offsetX, weightPerPixel) <- (-1 to 1).view.zip(weightPerRow)
                            (
                              inputPerChannel /* batchSize × height × width */,
                              weightPerChannel /* scalar */
                            ) <- inputSeq.view.zip(weightPerPixel)
                          } yield {

                            weightPerChannel.shape should be(empty)

                            inputPerChannel.translate(Array(0, offsetY, offsetX)) *
                              weightPerChannel.broadcast(Array(batchSize, height, width))
                          }

                          biasPerFilter.broadcast(Array(batchSize, height, width)) + summands.reduce(_ + _)
                      }

                    zip(outputChannels)
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

      val inputArray = Array.ofDim[Float](2, 4, 5, 3) /* batchSize × height × width × depth */

      inputArray(0)(0)(0)(0) = 1.0f
      inputArray(0)(1)(0)(0) = 10.0f
      inputArray(1)(0)(0)(0) = 100.0f

      val weightArray = Array.ofDim[Float](3, 3, 3, 2) /* kernelHeight × kernelWidth × depth × filterSize */
      weightArray(1)(1)(0)(0) = 3.0f
      weightArray(1)(1)(0)(1) = 4.0f
      weightArray(0)(1)(0)(0) = 5.0f
      weightArray(2)(2)(0)(1) = 6.0f

      val biasArray = Array[Float](100000.0f, 200000.0f) /* filterSize */

      val inputTensor = Tensor(inputArray)
      inputTensor.shape should be(Array(2, 4, 5, 3))
      val outputTensor = convolute(
        input = inputTensor,
        weight = Tensor(weightArray),
        bias = Tensor(biasArray)
      )
      outputTensor.shape should be(Array(2, 4, 5, 2)) /* batchSize × height × width × filterSize */

      Do.garbageCollected(outputTensor.flatArray).map { a =>
        val outputArray = a.grouped(2).toArray.grouped(5).toArray.grouped(4).toArray
        outputArray.length should be(2)

        outputArray(0)(0)(0)(0) should be(100053.0f)
        outputArray(0)(1)(1)(1) should be(200006.0f)
        outputArray(1)(1)(1)(1) should be(200600.0f)
        outputArray(0)(2)(1)(1) should be(200060.0f)
        outputArray(0)(0)(0)(1) should be(200004.0f)
        outputArray(0)(1)(0)(0) should be(100030.0f)
        outputArray(1)(0)(0)(0) should be(100300.0f)
      }

    }
  }.run.toScalaFuture

  "sum" in doTensors
    .map { tensors =>
      import tensors._
      Tensor.fill(15625.0f, Array(8, 8)).sum.toString should be("1000000.0")
    }
    .run
    .toScalaFuture

  "random" in doTensors
    .map { tensors =>
      import tensors._
      Tensor.random(Array(3, 3), seed = 12345).toString should be(
        "[[0.48931676,0.2949697,0.14271837],[0.9694414,0.26660874,0.07228618],[0.8779875,0.7046564,0.018829918]]")
    }
    .run
    .toScalaFuture

  "randomNormal" in doTensors
    .map { tensors =>
      import tensors._
      Tensor.randomNormal(Array(3, 3, 3), seed = 54321).toString should be(
        "[" +
          "[" +
          "[1.4561316,-0.8711971,-0.7223376]," +
          "[-2.232667,-0.24489015,-0.41490105]," +
          "[-1.0286478,-1.392045,0.08673929]" +
          "]," +
          "[" +
          "[-0.37037173,0.5294154,-0.5261399]," +
          "[-0.88834476,-0.66154,0.7035836]," +
          "[-1.1797824,-0.93145895,-1.0812063]" +
          "]," +
          "[" +
          "[-1.881317,0.20438789,-2.5961785]," +
          "[1.3082669,0.58748704,-0.01997061]," +
          "[-1.7090794,1.0162057,0.33355764]" +
          "]" +
          "]")
    }
    .run
    .toScalaFuture

  "matrix multiplication" in doTensors
    .map { tensors =>
      import tensors._

      def matrixMultiply(matrix1: Tensor, matrix2: Tensor): Tensor = {
        val Array(i, j) = matrix1.shape
        val Array(`j`, k) = matrix2.shape
        val product = matrix1.broadcast(Array(i, j, k)) * matrix2.reshape(Array(1, j, k)).broadcast(Array(i, j, k))

        product.unzip(1).reduce(_ + _)

      }

      val matrix1 = Tensor(Array(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f)))
      val matrix2 = Tensor(
        Array(Array(7.0f, 8.0f, 9.0f, 10.0f), Array(11.0f, 12.0f, 13.0f, 14.0f), Array(15.0f, 16.0f, 17.0f, 18.0f)))

      matrixMultiply(matrix1, matrix2).toString should be("[[74.0,80.0,86.0,92.0],[173.0,188.0,203.0,218.0]]")

    }
    .run
    .toScalaFuture

  "broadcast" in doTensors
    .map { tensors =>
      import tensors._

      val matrix1 = Tensor(Array(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f)))
      matrix1.broadcast(Array(2, 3, 4)).toString should be(
        "[[[1.0,1.0,1.0,1.0],[2.0,2.0,2.0,2.0],[3.0,3.0,3.0,3.0]],[[4.0,4.0,4.0,4.0],[5.0,5.0,5.0,5.0],[6.0,6.0,6.0,6.0]]]")
    }
    .run
    .toScalaFuture

  "unrolled matrix multiplication" in doTensors
    .map { tensors =>
      import tensors._

      def matrixMultiply(matrix1: Tensor, matrix2: Tensor): Tensor = {

        val columns1 = matrix1.unzip(1)

        Tensor.zip(matrix2.unzip(1).map { column2: Tensor =>
          (columns1 zip column2.unzip(0))
            .map {
              case (l: Tensor, r: Tensor) =>
                l * r.broadcast(l.shape)
            }
            .reduce[Tensor](_ + _)
        })
      }

      matrixMultiply(
        Tensor(Array(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f))),
        Tensor(
          Array(Array(7.0f, 8.0f, 9.0f, 10.0f), Array(11.0f, 12.0f, 13.0f, 14.0f), Array(15.0f, 16.0f, 17.0f, 18.0f)))
      ).toString should be("[[74.0,80.0,86.0,92.0],[173.0,188.0,203.0,218.0]]")

    }
    .run
    .toScalaFuture

}
