package com.thoughtworks.compute

import com.thoughtworks.feature.Factory
import com.typesafe.scalalogging.StrictLogging
import org.lwjgl.opencl.CL10.CL_DEVICE_TYPE_CPU

object gpu {

  val tensors = Factory[
    StrictLogging with Tensors.UnsafeMathOptimizations with Tensors.SuppressWarnings with OpenCL.LogContextNotification with OpenCL.GlobalExecutionContext with OpenCL.CommandQueuePool with OpenCL.UseAllGpuDevices with OpenCL.DontReleaseEventTooEarly with OpenCL.SynchronizedCreatingKernel with OpenCL.HandleEventInExecutionContext with Tensors.WangHashingRandomNumberGenerator]
    .newInstance(numberOfCommandQueuesPerDevice = 5)

}
