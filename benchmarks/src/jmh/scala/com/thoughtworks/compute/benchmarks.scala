package com.thoughtworks.compute

import java.util.concurrent.TimeUnit

import com.thoughtworks.compute.OpenCL.Exceptions.DeviceNotFound
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.continuation._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import com.thoughtworks.tryt.covariant._
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CL10
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.convolution.Convolution
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.ops.transforms.Transforms
import org.openjdk.jmh.annotations._
import scalaz.syntax.all._
import scalaz.std.list._

import scala.util.Try

object benchmarks {

  trait TensorState {
    @Param(Array("GPU"))
    protected var tensorDeviceType: String = _

    trait BenchmarkTensors
        extends StrictLogging
        with Tensors.UnsafeMathOptimizations
        with Tensors.SuppressWarnings
        with OpenCL.LogContextNotification
        with OpenCL.GlobalExecutionContext
        with OpenCL.CommandQueuePool
        with OpenCL.DontReleaseEventTooEarly
        with OpenCL.SynchronizedCreatingKernel
        with OpenCL.HandleEventInExecutionContext
        with Tensors.WangHashingRandomNumberGenerator {

      protected val numberOfCommandQueuesPerDevice: Int = 3

      @transient
      protected lazy val (platformId: PlatformId, deviceIds: Seq[DeviceId]) = {
        val deviceType = classOf[CL10].getField(s"CL_DEVICE_TYPE_$tensorDeviceType").get(null).asInstanceOf[Int]

        object MatchDeviceType {
          def unapply(platformId: PlatformId): Option[Seq[DeviceId]] = {
            (try {
              platformId.deviceIdsByType(deviceType)
            } catch {
              case e: DeviceNotFound =>
                return None
            }) match {
              case devices if devices.nonEmpty =>
                Some(devices)
              case _ =>
                None
            }

          }
        }

        platformIds.collectFirst {
          case platformId @ MatchDeviceType(deviceIds) =>
            (platformId, deviceIds)
        } match {
          case None =>
            throw new DeviceNotFound(s"$tensorDeviceType device is not found")
          case Some(pair) =>
            pair
        }
      }

    }
  }

  @Threads(value = Threads.MAX)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  @Timeout(time = 2, timeUnit = TimeUnit.SECONDS)
  @Fork(1)
  @State(Scope.Benchmark)
  class MatrixMultiplication extends TensorState {
    @Param(Array("8", "32"))
    protected var inputDepth: Int = _

    @Param(Array("8", "32"))
    protected var outputDepth: Int = _

    @Param(Array("65536", "4096", "32"))
    protected var batchSize: Int = _

    @transient
    private lazy val input = Nd4j.randn(Array(batchSize, inputDepth))

    @transient
    private lazy val weight = Nd4j.randn(Array(inputDepth, outputDepth))

    @Benchmark
    final def nd4j(): Array[Float] = {
      input.mmul(weight).data().asFloat()
    }

    trait Benchmarks extends BenchmarkTensors {

      @transient
      private lazy val unrollThreshold = deviceIds.head.maxComputeUnits * 128

      def matrixMultiply(matrix1: Tensor, matrix2: Tensor): Tensor = {
        val Array(i, j) = matrix1.shape
        if (i >= unrollThreshold) {
          // unroll j and k
          val columns1 = matrix1.unzip(1)
          Tensor.zip(matrix2.unzip(1).map { column2: Tensor =>
            (columns1 zip column2.unzip(0))
              .map {
                case (l: Tensor, r: Tensor) =>
                  l * r.broadcast(l.shape)
              }
              .reduce[Tensor](_ + _)
          })
        } else {
          // unroll only j
          val Array(`j`, k) = matrix2.shape
          val product = matrix1.broadcast(Array(i, j, k)) * matrix2.reshape(Array(1, j, k)).broadcast(Array(i, j, k))
          product.unzip(1).reduce(_ + _)
        }
      }

      def doBenchmark(): Do[() => Array[Float]] = {
        val weight: BufferedTensor = Tensor.randomNormal(Array(inputDepth, outputDepth))

        val input: BufferedTensor = Tensor.randomNormal(Array(batchSize, inputDepth))

        weight.doBuffer.flatMap { _ =>
          input.doBuffer.map { _ =>
            { () =>
              matrixMultiply(input, weight).flatArray.run.blockingAwait
            }
          }
        }
      }
    }

    private var benchmarkResource: Resource[UnitContinuation, Try[() => Array[Float]]] = _

    @Setup
    final def setup(): Unit = {
      //      Configuration.OPENCL_LIBRARY_NAME.set("/opt/pocl-1.1/lib/libOpenCL.dylib")
      assert(benchmarkResource == null)
      val Do(TryT(ResourceT(resourceContinuation))) =
        Do.monadicCloseable(Factory[Benchmarks].newInstance()).flatMap(_.doBenchmark())
      benchmarkResource = resourceContinuation.blockingAwait()
    }

    @TearDown(Level.Trial)
    final def tearDown(): Unit = {
      val benchmarkResource = this.benchmarkResource
      this.benchmarkResource = null
      benchmarkResource.release.blockingAwait
    }

    @Benchmark
    final def computeDotScala(): Array[Float] = {
      benchmarkResource.value.get.apply()
    }
  }

  @Threads(value = Threads.MAX)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  @Timeout(time = 2, timeUnit = TimeUnit.SECONDS)
  @Fork(1)
  @State(Scope.Benchmark)
  class Tanh extends TensorState {

    @transient
    private lazy val input = Nd4j.randn(Array.fill(numberOfDimensions)(size))

    @Benchmark
    final def nd4j(): Array[Float] = {
      (0 until numberOfIterations)
        .foldLeft(input) { (input, _) =>
          Transforms.tanh(input)
        }
        .data()
        .asFloat()
    }

    trait Benchmarks extends BenchmarkTensors {

      def doBenchmark(): Do[() => Array[Float]] = {
        val input = Tensor.randomNormal(Array.fill(numberOfDimensions)(size))

        input.doBuffer.map { _ =>
          { () =>
            (0 until numberOfIterations)
              .foldLeft[Tensor](input) { (input, _) =>
                Tensor.tanh(input)
              }
              .flatArray
              .run
              .blockingAwait
          }
        }
      }
    }

    private var benchmarkResource: Resource[UnitContinuation, Try[() => Array[Float]]] = _

    @Setup
    final def setup(): Unit = {
      assert(benchmarkResource == null)
      val Do(TryT(ResourceT(resourceContinuation))) =
        Do.monadicCloseable(Factory[Benchmarks].newInstance()).flatMap(_.doBenchmark())
      benchmarkResource = resourceContinuation.blockingAwait()
    }

    @TearDown(Level.Trial)
    final def tearDown(): Unit = {
      val benchmarkResource = this.benchmarkResource
      this.benchmarkResource = null
      benchmarkResource.release.blockingAwait
    }

    @Benchmark
    final def computeDotScala(): Array[Float] = {
      benchmarkResource.value.get.apply()
    }

    @Param(Array("100", "10", "1"))
    protected var numberOfIterations: Int = _

    @Param(Array("2", "3"))
    protected var numberOfDimensions: Int = _

    @Param(Array("128", "32"))
    protected var size: Int = _
  }

  @Threads(value = Threads.MAX)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  @Timeout(time = 2, timeUnit = TimeUnit.SECONDS)
  @Fork(1)
  @State(Scope.Benchmark)
  class Sum extends TensorState {

    @transient
    private lazy val input = Nd4j.randn(Array.fill(numberOfDimensions)(size))

    @Benchmark
    final def nd4j(): Float = {
      input.sumNumber().floatValue()
    }

    trait Benchmarks extends BenchmarkTensors {

      def doBenchmark(): Do[() => Float] = {
        val input: BufferedTensor = Tensor.randomNormal(Array.fill(numberOfDimensions)(size))

        input.doBuffer.map { _ =>
          { () =>
            val Array(v) = input.sum.flatArray.run.blockingAwait
            v
          }
        }
      }
    }

    private var benchmarkResource: Resource[UnitContinuation, Try[() => Float]] = _

    @Setup
    final def setup(): Unit = {
      assert(benchmarkResource == null)
      val Do(TryT(ResourceT(resourceContinuation))) =
        Do.monadicCloseable(Factory[Benchmarks].newInstance()).flatMap(_.doBenchmark())
      benchmarkResource = resourceContinuation.blockingAwait()
    }

    @TearDown(Level.Trial)
    final def tearDown(): Unit = {
      val benchmarkResource = this.benchmarkResource
      this.benchmarkResource = null
      benchmarkResource.release.blockingAwait
    }

    @Benchmark
    final def computeDotScala(): Float = {
      benchmarkResource.value.get.apply()
    }

    @Param(Array("3", "2", "1"))
    protected var numberOfDimensions: Int = _

    @Param(Array("128", "32", "16"))
    protected var size: Int = _
  }

  @Threads(value = Threads.MAX)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  @Timeout(time = 2, timeUnit = TimeUnit.SECONDS)
  @Fork(1)
  @State(Scope.Benchmark)
  class RandomNormal extends TensorState {

    @Param(Array("3", "2", "1"))
    protected var numberOfDimensions: Int = _

    @Param(Array("128", "32", "16"))
    protected var size: Int = _

    @Benchmark
    final def nd4j(): Array[Float] = {
      Nd4j.randn(Array.fill(numberOfDimensions)(size)).data().asFloat()
    }
    trait Benchmarks extends BenchmarkTensors {}

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
    final def computeDotScala(): Array[Float] = {
      benchmarks.Tensor.randomNormal(Array.fill(numberOfDimensions)(size)).flatArray.run.blockingAwait
    }
  }

  @Threads(value = Threads.MAX)
  @Warmup(iterations = 5)
  @Measurement(iterations = 5)
  @Timeout(time = 2, timeUnit = TimeUnit.SECONDS)
  @Fork(1)
  @State(Scope.Benchmark)
  class Convolution extends TensorState {

    @transient
    private lazy val input = Nd4j.randn(Array(batchSize, depth, imageHeight, imageWidth))

    @transient
    private lazy val layers = (for (i <- (0 until numberOfLayers).view) yield {
      (Nd4j.randn(Array(depth, kernelHeight, kernelWidth, depth)), Nd4j.randn(Array(depth)))
    }).toList

    private def conv2d(input: INDArray /* batchSize × depth × height × width */,
                       weight: INDArray /* filterSize × kernelHeight × kernelWidth × depth */,
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
    final def nd4j(): Array[Float] = {
      layers
        .foldLeft(input) { (input, layer) =>
          val (weight, bias) = layer
          conv2d(input, weight, bias)
        }
        .data()
        .asFloat()
    }

    trait Benchmarks extends BenchmarkTensors {

      final case class ConvolutionalLayer(weight: BufferedTensor, bias: BufferedTensor) {
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

      def doBenchmark(): Do[() => Array[Float]] = {
        val input: BufferedTensor = Tensor.randomNormal(Array(batchSize, imageHeight, imageWidth, depth))
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
                .foldLeft[Tensor](input) { (input, layer) =>
                  layer.forward(input)
                }
                .flatArray
                .run
                .blockingAwait

            }
        }
      }

    }

    private var benchmarkResource: Resource[UnitContinuation, Try[() => Array[Float]]] = _

    @Setup
    final def setup(): Unit = {
      assert(benchmarkResource == null)
      val Do(TryT(ResourceT(resourceContinuation))) =
        Do.monadicCloseable(Factory[Benchmarks].newInstance()).flatMap(_.doBenchmark())
      benchmarkResource = resourceContinuation.blockingAwait()
    }

    @TearDown(Level.Trial)
    final def tearDown(): Unit = {
      val benchmarkResource = this.benchmarkResource
      this.benchmarkResource = null
      benchmarkResource.release.blockingAwait
    }

    @Benchmark
    final def computeDotScala(): Array[Float] = {
      benchmarkResource.value.get.apply()
    }

    // ND4J is too slow when increasing the number of layers.
    @Param(Array("1"))
    protected var numberOfLayers: Int = _

    @Param(Array("3", "1"))
    protected var kernelWidth: Int = _

    protected def kernelHeight: Int = kernelWidth

    @Param(Array("32"))
    protected var imageHeight: Int = _

    @Param(Array("32"))
    protected var imageWidth: Int = _

    @Param(Array("128", "32"))
    protected var batchSize: Int = _

    @Param(Array("8", "3"))
    protected var depth: Int = _

  }

}
