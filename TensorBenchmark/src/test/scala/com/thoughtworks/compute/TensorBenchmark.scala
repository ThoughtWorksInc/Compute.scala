package com.thoughtworks.compute

import com.thoughtworks.feature.Factory
import com.thoughtworks.future._
import com.thoughtworks.continuation._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.raii.covariant._
import com.thoughtworks.tryt.covariant._
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CLCapabilities
import org.openjdk.jmh.annotations._
import scalaz.syntax.all._
import scalaz.std.list._

import scala.util.Try

/**
  * @author 杨博 (Yang Bo)
  */
@State(Scope.Benchmark)
@Threads(value = Threads.MAX)
class TensorBenchmark {
  import com.thoughtworks.compute.TensorBenchmark._

  @Param(Array("5", "1", "2", "10"))
  private var numberOfLayers: Int = 3

  trait Benchmarks
      extends StrictLogging
      with OpenCL.LogContextNotification
      with OpenCL.GlobalExecutionContext
      with OpenCL.UseAllDevices
      with OpenCL.UseFirstPlatform
      with OpenCL.CommandQueuePool
      with OpenCL.DontReleaseEventTooEarly
      with Tensors.WangHashingRandomNumberGenerator
      with Convolution {

    protected val numberOfCommandQueuesForDevice: (Long, CLCapabilities) => Int = { (_, _) =>
      3
    }

    override def monadicClose: UnitContinuation[Unit] = {
      super.monadicClose
    }

    def doComputeConvolution(): Do[() => Array[Float]] = {
      val input = Tensor.random(Array(32, 32, 32, 3))
      val layers = (for (i <- (0 until numberOfLayers).view) yield {
        ConvolutionalLayer(weight = Tensor.random(Array(3, 3, 3, 3)), bias = Tensor.random(Array(3)))
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
  def setup(): Unit = {
    assert(benchmarkResouce == null)
    val Do(TryT(ResourceT(resourceContinuation))) =
      Do.monadicCloseable(Factory[Benchmarks].newInstance()).flatMap(_.doComputeConvolution())
    benchmarkResouce = resourceContinuation.blockingAwait()
  }

  @TearDown(Level.Trial)
  def tearDown(): Unit = {
    val benchmarkResouce = this.benchmarkResouce
    this.benchmarkResouce = null
    benchmarkResouce.release.blockingAwait
  }

  @Benchmark
  def computeConvolution(): Array[Float] = {
    benchmarkResouce.value.get.apply()
  }

}

object TensorBenchmark {

  trait Convolution extends Tensors {
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
