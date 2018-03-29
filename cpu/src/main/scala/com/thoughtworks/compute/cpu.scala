package com.thoughtworks.compute

import com.thoughtworks.feature.Factory
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CL10.CL_DEVICE_TYPE_CPU

/** Contains N-dimensional array types on GPU.
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
  */
object cpu
    extends StrictLogging
    with Tensors.UnsafeMathOptimizations
    with Tensors.SuppressWarnings
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
