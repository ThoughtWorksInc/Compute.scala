package com.thoughtworks.compute

import com.typesafe.scalalogging.StrictLogging

/** Contains N-dimensional array types on CPU.
  *
  * All the usage of this [[gpu]] object is same as [[cpu]], except the `import` statement:
  *
  * {{{
  * import com.thoughtworks.compute.gpu._
  * }}}
  *
  * @see [[cpu]] for Usage.
  */
object gpu
    extends StrictLogging
    with Tensors.UnsafeMathOptimizations
    with OpenCL.LogContextNotification
    with OpenCL.GlobalExecutionContext
    with OpenCL.CommandQueuePool
    with OpenCL.UseAllGpuDevices
    with OpenCL.DontReleaseEventTooEarly
    with OpenCL.SynchronizedCreatingKernel
    with OpenCL.HandleEventInExecutionContextForIntelAndAMDPlatform
    with Tensors.WangHashingRandomNumberGenerator {
  protected val numberOfCommandQueuesPerDevice: Int = 5
}
