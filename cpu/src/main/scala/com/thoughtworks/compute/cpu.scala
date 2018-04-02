package com.thoughtworks.compute

import com.thoughtworks.feature.Factory
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CL10.CL_DEVICE_TYPE_CPU

/** Contains N-dimensional array types on CPU.
  *
  * You may want to import [[Tensor]], which is the base type of N-dimensional arrays:
  *
  * {{{
  * import com.thoughtworks.compute.cpu.Tensor
  * }}}
  *
  * @example In Compute.scala, an N-dimensional array is typed as [[Tensor]],
  *          which can be created from [[scala.collection.Seq]] or [[scala.Array]].
  *
  *          {{{
  *          val my2DArray: Tensor = Tensor(Array(Seq(1.0f, 2.0f), Seq(3.0f, 4.0f)))
  *
  *          my2DArray.toString should be("[[1.0,2.0],[3.0,4.0]]")
  *          }}}
  *
  * @example A `Tensor` can be `split` into small `Tensor`s on the direction of a specific dimension.
  *
  *          Given a 3D tensor whose `shape` is 2x3x4,
  *
  *          {{{
  *          val my3DTensor = Tensor((0.0f until 24.0f by 1.0f).grouped(4).toSeq.grouped(3).toSeq)
  *          my3DTensor.shape should be(Array(2, 3, 4))
  *          }}}
  *
  *          when `split` it at the dimension #0,
  *
  *          {{{
  *          val subtensors0 = my3DTensor.split(dimension = 0)
  *          }}}
  *
  *          then the result should be a `Seq` of two 3x4 tensors.
  *
  *          {{{
  *          subtensors0.toString should be("TensorSeq([[0.0,1.0,2.0,3.0],[4.0,5.0,6.0,7.0],[8.0,9.0,10.0,11.0]], [[12.0,13.0,14.0,15.0],[16.0,17.0,18.0,19.0],[20.0,21.0,22.0,23.0]])")
  *
  *          inside(subtensors0) {
  *            case Seq(subtensor0, subtensor1) =>
  *              subtensor0.shape should be(Array(3, 4))
  *              subtensor1.shape should be(Array(3, 4))
  *          }
  *          }}}
  *
  *          When `split` it at the dimension #1,
  *
  *          {{{
  *          val subtensors1 = my3DTensor.split(dimension = 1)
  *          }}}
  *
  *          then the result should be a `Seq` of three 2x4 tensors.
  *
  *          {{{
  *          subtensors1.toString should be("TensorSeq([[0.0,1.0,2.0,3.0],[12.0,13.0,14.0,15.0]], [[4.0,5.0,6.0,7.0],[16.0,17.0,18.0,19.0]], [[8.0,9.0,10.0,11.0],[20.0,21.0,22.0,23.0]])")
  *
  *          inside(subtensors1) {
  *            case Seq(subtensor0, subtensor1, subtensor2) =>
  *              subtensor0.shape should be(Array(2, 4))
  *              subtensor1.shape should be(Array(2, 4))
  *              subtensor2.shape should be(Array(2, 4))
  *          }
  *          }}}
  *
  * @example Multiple `Tensor`s of the same `shape` can be merged into a larger `Tensor` via the `Tensor.join` function.
  *
  *          Given a `Seq` of three 2x2 `Tensor`s,
  *
  *          {{{
  *          val mySubtensors: Seq[Tensor] = Seq(
  *            Tensor(Seq(Seq(1.0f, 2.0f), Seq(3.0f, 4.0f))),
  *            Tensor(Seq(Seq(5.0f, 6.0f), Seq(7.0f, 8.0f))),
  *            Tensor(Seq(Seq(9.0f, 10.0f), Seq(11.0f, 12.0f))),
  *          )
  *          }}}
  *
  *          when `join`ing them,
  *          {{{
  *          val merged: Tensor = Tensor.join(mySubtensors)
  *          }}}
  *
  *          then the result should be a 2x2x3 `Tensor`.
  *
  *          {{{
  *          merged.toString should be("[[[1.0,5.0,9.0],[2.0,6.0,10.0]],[[3.0,7.0,11.0],[4.0,8.0,12.0]]]")
  *          merged.shape should be(Array(2, 2, 3))
  *          }}}
  *
  *
  */
object cpu
    extends StrictLogging
    with Tensors.UnsafeMathOptimizations
    with OpenCL.LogContextNotification
    with OpenCL.GlobalExecutionContext
    with OpenCL.CommandQueuePool
    with OpenCL.UseAllCpuDevices
    with OpenCL.DontReleaseEventTooEarly
    with OpenCL.SynchronizedCreatingKernel
    with OpenCL.HandleEventInExecutionContextForIntelAndAMDPlatform
    with Tensors.WangHashingRandomNumberGenerator {

  protected val numberOfCommandQueuesPerDevice = 5

}
