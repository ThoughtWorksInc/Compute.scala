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
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

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
        for (i <- floatBuffer.position() until floatBuffer.limit()) {
          floatBuffer.get(i) should be(element)
        }
        floatBuffer.remaining() should be(shape.product)
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
      }
    }
  }.run.toScalaFuture

  "unzip" in {
    doTensors.map { tensors =>
      import tensors._
      val tensor = Tensor(Seq(Seq(Seq(Seq(1.0f, 5.0f)))))
      tensor.split(dimension = 3).map(_.toString) should be(Seq("[[[1.0]]]", "[[[5.0]]]"))
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
      import tensors.Tensor.join
      def convolute(input: Tensor /* batchSize × height × width × depth */,
                    weight: Tensor /* kernelHeight × kernelWidth × depth × filterSize */,
                    bias: Tensor /* filterSize */ ): Tensor = {
        input.shape match {
          case Array(batchSize, height, width, depth) =>
            weight.shape match {
              case Array(kernelHeight, kernelWidth, `depth`, filterSize) =>
                bias.shape match {
                  case Array(`filterSize`) =>
                    val inputSeq: Seq[Tensor /* batchSize × height × width */ ] = input.split(dimension = 3)

                    inputSeq.size should be(depth)
                    inputSeq.head.shape should be(Array(batchSize, height, width))

                    val weightSeq: Seq[Seq[Seq[Seq[Tensor]]]] /* filterSize × kernelHeight × kernelWidth × depth */ =
                      weight.split(dimension = 3).map { khKwD =>
                        khKwD.shape should be(Array(kernelHeight, kernelWidth, depth))

                        khKwD.split(dimension = 0).map { kwD =>
                          kwD.shape should be(Array(kernelWidth, depth))
                          kwD.split(dimension = 0).map { d =>
                            d.shape should be(Array(depth))
                            d.split(dimension = 0)
                          }
                        }
                      }

                    weightSeq.length should be(filterSize)
                    weightSeq.head.length should be(kernelHeight)
                    weightSeq.head.head.length should be(kernelWidth)
                    weightSeq.head.head.head.length should be(depth)

                    val biasSeq: Seq[Tensor] /* filterSize */ = bias.split(dimension = 0)

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

                    join(outputChannels)
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

  "randomNormal scalar" in doTensors
    .flatMap { tensors =>
      import tensors._
      Do.garbageCollected(Tensor.randomNormal(Array.empty, seed = 54321).flatArray.map(_ should be(Array(1.4561316f))))
    }
    .run
    .toScalaFuture
  
  "readScalar" in doTensors
    .flatMap { tensors =>
      Do.garbageCollected(tensors.Tensor(42.0f).readScalar).map {a=>
        a should be(42.0f)
      }
    }
    .run
    .toScalaFuture

  "read1DArray" in doTensors
    .flatMap { tensors =>
      Do.garbageCollected(tensors.Tensor(Array[Float](1,2)).read1DArray).map {a=>
        a should be(Array[Float](1,2))
      }
    }
    .run
    .toScalaFuture

  "read2DArray" in doTensors
    .flatMap { tensors =>
      import tensors._
      val array = Array(Array[Float](1, 2), Array[Float](3, 4), Array[Float](5,6))
      Do.garbageCollected(Tensor(array).read2DArray).map {a=>
        a(0) should be(Array[Float](1, 2))
        a(1) should be(Array[Float](3, 4))
        a(2) should be(Array[Float](5, 6))
      }
    }
    .run
    .toScalaFuture

  "read3DArray" in doTensors
    .flatMap { tensors =>
      import tensors._
      val array = Array(Array(Array[Float](1, 2), Array[Float](3, 4), Array[Float](5,6)), Array(Array[Float](7, 8), Array[Float](9, 10), Array[Float](11,12)))
      Do.garbageCollected(Tensor(array).read3DArray).map { a =>
        a(0)(0) should be(Array[Float](1,2))
        a(0)(1) should be(Array[Float](3,4))
        a(0)(2) should be(Array[Float](5,6))
        a(1)(0) should be(Array[Float](7,8))
        a(1)(1) should be(Array[Float](9,10))
        a(1)(2) should be(Array[Float](11,12))
      }
    }
    .run
    .toScalaFuture

  "read4DArray" in doTensors
    .flatMap { tensors =>
      import tensors._
      val array = Array(Array(Array(Array[Float](1, 2), Array[Float](3, 4), Array[Float](5,6)),
                        Array(Array[Float](7, 8), Array[Float](9, 10), Array[Float](11,12))),
                        Array(Array(Array[Float](13, 14), Array[Float](15, 16), Array[Float](17,18)),
                        Array(Array[Float](19, 20), Array[Float](21, 22), Array[Float](23,24))))
      Do.garbageCollected(Tensor(array).read4DArray).map { a =>
        a(0)(0)(0) should be(Array[Float](1,2))
        a(0)(0)(1) should be(Array[Float](3,4))
        a(0)(0)(2) should be(Array[Float](5,6))
        a(0)(1)(0) should be(Array[Float](7,8))
        a(0)(1)(1) should be(Array[Float](9,10))
        a(0)(1)(2) should be(Array[Float](11,12))
        a(1)(0)(0) should be(Array[Float](13,14))
        a(1)(0)(1) should be(Array[Float](15,16))
        a(1)(0)(2) should be(Array[Float](17,18))
        a(1)(1)(0) should be(Array[Float](19,20))
        a(1)(1)(1) should be(Array[Float](21,22))
        a(1)(1)(2) should be(Array[Float](23,24))
      }
    }
    .run
    .toScalaFuture

   "read1DSeq" in doTensors
    .flatMap { tensors =>
      Do.garbageCollected(tensors.Tensor(Seq[Float](1,2)).read1DSeq).map {a=>
        a should be(Seq[Float](1,2))
      }
    }
    .run
    .toScalaFuture

  "read2DSeq" in doTensors
    .flatMap { tensors =>
      import tensors._
      val seq = Seq(Seq[Float](1, 2), Seq[Float](3, 4), Seq[Float](5,6))
      Do.garbageCollected(Tensor(seq).read2DSeq).map {a=>
        a(0) should be(Seq[Float](1, 2))
        a(1) should be(Seq[Float](3, 4))
        a(2) should be(Seq[Float](5, 6))
      }
    }
    .run
    .toScalaFuture

  "read3DSeq" in doTensors
    .flatMap { tensors =>
      import tensors._
      val seq = Seq(Seq(Seq[Float](1, 2), Seq[Float](3, 4), Seq[Float](5,6)), Seq(Seq[Float](7, 8), Seq[Float](9, 10), Seq[Float](11,12)))
      Do.garbageCollected(Tensor(seq).read3DSeq).map { a =>
        a(0)(0) should be(Seq[Float](1,2))
        a(0)(1) should be(Seq[Float](3,4))
        a(0)(2) should be(Seq[Float](5,6))
        a(1)(0) should be(Seq[Float](7,8))
        a(1)(1) should be(Seq[Float](9,10))
        a(1)(2) should be(Seq[Float](11,12))
      }
    }
    .run
    .toScalaFuture

  "read4DSeq" in doTensors
    .flatMap { tensors =>
      import tensors._
      val seq = Seq(Seq(Seq(Seq[Float](1, 2), Seq[Float](3, 4), Seq[Float](5,6)),
                        Seq(Seq[Float](7, 8), Seq[Float](9, 10), Seq[Float](11,12))),
                        Seq(Seq(Seq[Float](13, 14), Seq[Float](15, 16), Seq[Float](17,18)),
                        Seq(Seq[Float](19, 20), Seq[Float](21, 22), Seq[Float](23,24))))
      Do.garbageCollected(Tensor(seq).read4DSeq).map { a =>
        a(0)(0)(0) should be(Seq[Float](1,2))
        a(0)(0)(1) should be(Seq[Float](3,4))
        a(0)(0)(2) should be(Seq[Float](5,6))
        a(0)(1)(0) should be(Seq[Float](7,8))
        a(0)(1)(1) should be(Seq[Float](9,10))
        a(0)(1)(2) should be(Seq[Float](11,12))
        a(1)(0)(0) should be(Seq[Float](13,14))
        a(1)(0)(1) should be(Seq[Float](15,16))
        a(1)(0)(2) should be(Seq[Float](17,18))
        a(1)(1)(0) should be(Seq[Float](19,20))
        a(1)(1)(1) should be(Seq[Float](21,22))
        a(1)(1)(2) should be(Seq[Float](23,24))
      }
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

  "scalar transpose" in doTensors
    .map { tensors =>
      tensors.Tensor(42.0f).transpose.toString should be("42.0")
    }
    .run
    .toScalaFuture

  
  "1d transpose" in doTensors
    .map { tensors =>
      tensors.Tensor(Array(1.0f, 2.0f, 3.0f)).transpose.toString should be("[1.0,2.0,3.0]")
    }
    .run
    .toScalaFuture
  
  "2d transpose" in doTensors
    .map { tensors =>
      val matrix = tensors.Tensor(Array(Array(1.0f, 2.0f), Array(3.0f, 4.0f)))
      matrix.transpose.toString should be("[[1.0,3.0],[2.0,4.0]]")
    }
    .run
    .toScalaFuture

  "3d transpose" in doTensors
    .map { tensors =>
      val t = tensors.Tensor(Array(Array(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f)), 
                                Array(Array(7.0f, 8.0f, 9.0f), Array(10.0f, 11.0f, 12.0f))))
      t.transpose.toString should be("[[[1.0,7.0],[4.0,10.0]],[[2.0,8.0],[5.0,11.0]],[[3.0,9.0],[6.0,12.0]]]")
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

        product.split(1).reduce[Tensor](_ + _)

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
        val columns1 = matrix1.split(1)
        val columns2 = matrix2.split(1)
        val resultColumns = columns2.map { column2: Tensor =>
          (columns1.view zip column2.split(0))
            .map {
              case (l: Tensor, r: Tensor) =>
                l * r.broadcast(l.shape)
            }
            .reduce[Tensor](_ + _)
        }
        Tensor.join(resultColumns)
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
