package com.thoughtworks.compute

import com.thoughtworks.feature.Factory
import com.thoughtworks.raii.asynchronous.Do
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CLCapabilities
import org.openjdk.jmh.annotations.{Scope, State}

/**
  * @author 杨博 (Yang Bo)
  */
@State(Scope.Benchmark)
class TensorBenchmark {
  private def doTensors: Do[Tensors] =
    Do.monadicCloseable(Factory[
      StrictLogging with OpenCL.LogContextNotification with OpenCL.GlobalExecutionContext with OpenCL.UseAllDevices with OpenCL.UseFirstPlatform with OpenCL.CommandQueuePool with Tensors with OpenCL.DontReleaseEventTooEarly]
      .newInstance(
        numberOfCommandQueuesForDevice = { (deviceId: Long, capabilities: CLCapabilities) =>
          5
        }
      ))

}

object TensorBenchmark {

  trait Convolution extends Tensors {
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
                    case Seq(h, w, d, _*) if h.length == kernelHeight && w.length == kernelWidth && d.length == depth =>
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

  }

}
