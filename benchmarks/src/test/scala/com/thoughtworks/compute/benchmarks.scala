package com.thoughtworks.compute

import com.thoughtworks.compute.benchmarks.RandomNormalState
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.continuation._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import com.thoughtworks.tryt.covariant._
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CLCapabilities
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.convolution.Convolution
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.ops.transforms.Transforms
import org.openjdk.jmh.annotations._
import scalaz.syntax.all._
import scalaz.std.list._

import scala.concurrent.ExecutionContext
import scala.util.Try

object benchmarks {
  @Threads(value = Threads.MAX)
  @State(Scope.Benchmark)
  class Nd4jRandomNormal extends RandomNormalState {

    @Benchmark
    final def nd4jRandomNormalBenchmark(): Array[Float] = {
      Nd4j.randn(Array.fill(numberOfDimensions)(size)).data().asFloat()
    }
  }

  @Threads(value = Threads.MAX)
  @State(Scope.Benchmark)
  class TensorRandomNormal extends RandomNormalState {
    trait Benchmarks
        extends StrictLogging
        with OpenCL.LogContextNotification
        with OpenCL.GlobalExecutionContext
        with OpenCL.UseAllDevices
        with OpenCL.UseFirstPlatform
        with OpenCL.CommandQueuePool
        with OpenCL.DontReleaseEventTooEarly
        with Tensors.WangHashingRandomNumberGenerator {

      protected val numberOfCommandQueuesForDevice: (Long, CLCapabilities) => Int = { (_, _) =>
        2
      }
    }

    var benchmarks: Benchmarks = _

    @Setup
    final def setup(): Unit = {
      assert(benchmarks == null)
      benchmarks = Factory[Benchmarks].newInstance()
    }

    @TearDown(Level.Trial)
    final def tearDown(): Unit = {
      val benchmarks = this.benchmarks
      this.benchmarks = null
      benchmarks.monadicClose.blockingAwait

    }

    @Benchmark
    final def tensorRandomNormalBenchmark(): Array[Float] = {
      benchmarks.Tensor.randomNormal(Array.fill(numberOfDimensions)(size)).flatArray.run.blockingAwait
    }
  }

  trait RandomNormalState {

    @Param(Array("3", "2", "1"))
    protected var numberOfDimensions: Int = _

    @Param(Array("128", "64", "32", "16"))
    protected var size: Int = _

  }

  @Threads(value = Threads.MAX)
  @State(Scope.Benchmark)
  class Nd4jConvolution extends ConvolutionState {
    private val input = Nd4j.randn(Array(batchSize, depth, imageHeight, imageWidth))
    private val layers = (for (i <- (0 until numberOfLayers).view) yield {
      (Nd4j.randn(Array(kernelHeight, kernelWidth, depth, depth)), Nd4j.randn(Array(depth)))
    }).toList

    private def conv2d(input: INDArray /* batchSize × height × width × depth */,
                       weight: INDArray /* kernelHeight × kernelWidth × depth × filterSize */,
                       bias: INDArray): INDArray = {
      val Array(numberOfImages, depth, height, width) = input.shape()

      val numberOfKernels = weight.shape().head

      val col = Convolution.im2col(input, Array(kernelHeight, kernelWidth), Array(1, 1), Array(1, 1))
      val permutedCol = col.permute(0, 4, 5, 1, 2, 3)
      val depthKernelKernel = depth * kernelHeight * kernelWidth
      val operandCol2d = permutedCol.reshape(numberOfImages * height * width, depthKernelKernel)
      val reshapedWeight = weight.reshape(numberOfKernels, depthKernelKernel)
      val permutedWeight = reshapedWeight.permute(1, 0)

      val dotResult = operandCol2d.mmul(permutedWeight)
      val plusResult = dotResult.add(bias.broadcast(dotResult.shape(): _*))
      val reshapeResult = plusResult.reshape(numberOfImages, height, width, numberOfKernels)
      reshapeResult.permute(0, 3, 1, 2)

    }

    @Benchmark
    final def nd4jConv2dBenchmark(): Array[Float] = {
      layers
        .foldLeft(input) { (input, layer) =>
          val (weight, bias) = layer
          conv2d(input, weight, bias)
        }
        .data()
        .asFloat()
    }
  }

  @Threads(value = Threads.MAX)
  @State(Scope.Benchmark)
  class TensorConvolution extends ConvolutionState {

    trait Benchmarks
        extends StrictLogging
        with OpenCL.LogContextNotification
        with OpenCL.GlobalExecutionContext
        with OpenCL.UseAllDevices
        with OpenCL.UseFirstPlatform
        with OpenCL.CommandQueuePool
        with OpenCL.DontReleaseEventTooEarly
        with Tensors.WangHashingRandomNumberGenerator
        with ConvolutionTensors {

      protected val numberOfCommandQueuesForDevice: (Long, CLCapabilities) => Int = { (_, _) =>
        2
      }

      def doBenchmark(): Do[() => Array[Float]] = {
        val input = Tensor.randomNormal(Array(batchSize, imageHeight, imageWidth, depth))
        val layers = (for (i <- (0 until numberOfLayers).view) yield {
          ConvolutionalLayer(weight = Tensor.randomNormal(Array(kernelHeight, kernelWidth, depth, depth)),
                             bias = Tensor.randomNormal(Array(depth)))
        }).toList

        input.doBuffer.flatMap { _ =>
          layers
            .traverseM {
              case ConvolutionalLayer(weight, bias) =>
                weight.doBuffer.flatMap { weightBuffer =>
                  bias.doBuffer.map { biasBuffer =>
                    List(weightBuffer, biasBuffer)
                  }
                }
            }
            .map { _ => () =>
              layers
                .foldLeft(input) { (input, layer) =>
                  layer.forward(input)
                }
                .flatArray
                .run
                .blockingAwait

            }
        }
      }

    }

    private var benchmarkResouce: Resource[UnitContinuation, Try[() => Array[Float]]] = _

    @Setup
    final def setup(): Unit = {
      assert(benchmarkResouce == null)
      val Do(TryT(ResourceT(resourceContinuation))) =
        Do.monadicCloseable(Factory[Benchmarks].newInstance()).flatMap(_.doBenchmark())
      benchmarkResouce = resourceContinuation.blockingAwait()
    }

    @TearDown(Level.Trial)
    final def tearDown(): Unit = {
      val benchmarkResouce = this.benchmarkResouce
      this.benchmarkResouce = null
      benchmarkResouce.release.blockingAwait
    }

    @Benchmark
    final def tensorConv2dBenchmark(): Array[Float] = {
      benchmarkResouce.value.get.apply()
    }

  }

  trait ConvolutionState {
    @Param(Array("5", "1", "2", "10"))
    protected var numberOfLayers: Int = _

    @Param(Array("3", "1"))
    protected var kernelWidth: Int = _

    @Param(Array("3", "1"))
    protected var kernelHeight: Int = _

    @Param(Array("32", "64"))
    protected var imageHeight: Int = _

    @Param(Array("32", "64"))
    protected var imageWidth: Int = _

    @Param(Array("32", "64"))
    protected var batchSize: Int = _

    @Param(Array("3", "10", "20"))
    protected var depth: Int = _

  }

  trait ConvolutionTensors extends Tensors {
    final case class ConvolutionalLayer(weight: Tensor, bias: Tensor) {
      def forward(input: Tensor): Tensor = {
        convolute(input, weight, bias)
      }
    }

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

                  if (inputSeq.size != depth) {
                    throw new IllegalArgumentException
                  }

                  inputSeq.head.shape match {
                    case Array(batchSize, height, width) =>
                    case _ =>
                      throw new IllegalArgumentException
                  }

                  val weightSeq: Seq[Seq[Seq[Seq[Tensor]]]] /* filterSize × kernelHeight × kernelWidth × depth */ =
                    weight.unzip(dimension = 3).map { khKwD =>
                      khKwD.shape match {
                        case Array(kernelHeight, kernelWidth, depth) =>
                        case _ =>
                          throw new IllegalArgumentException
                      }

                      khKwD.unzip(dimension = 0).map { kwD =>
                        kwD.shape match {
                          case Array(kernelWidth, depth) =>
                          case _ =>
                            throw new IllegalArgumentException
                        }

                        kwD.unzip(dimension = 0).map { d =>
                          d.shape match {
                            case Array(depth) =>
                            case _ =>
                              throw new IllegalArgumentException
                          }
                          d.unzip(dimension = 0)
                        }
                      }
                    }

                  weightSeq match {
                    case Seq(h @ Seq(w @ Seq(d, _*), _*), _*)
                        if h.length == kernelHeight && w.length == kernelWidth && d.length == depth =>
                    case _ =>
                      throw new IllegalArgumentException
                  }
                  if (weightSeq.length != filterSize) {
                    throw new IllegalArgumentException
                  }

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

                          if (weightPerChannel.shape.nonEmpty) {
                            throw new IllegalArgumentException
                          }

                          inputPerChannel.translate(Array(0, offsetY, offsetX)) *
                            weightPerChannel.broadcast(Array(batchSize, height, width))
                        }

                        biasPerFilter.broadcast(Array(batchSize, height, width)) + summands.reduce(_ + _)
                    }

                  Tensor.zip(outputChannels)
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

  }

}
