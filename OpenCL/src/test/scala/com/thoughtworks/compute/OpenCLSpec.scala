package com.thoughtworks.compute

import java.nio.ByteBuffer

import org.scalatest.{AsyncFreeSpec, Matchers}
import org.lwjgl.opencl._
import CL10._
import CL12._
import com.thoughtworks.raii.asynchronous._
import com.thoughtworks.each.Monadic._
import com.thoughtworks.feature.Factory
import com.thoughtworks.future._

/**
  * @author 杨博 (Yang Bo)
  */
final class OpenCLSpec extends AsyncFreeSpec with Matchers {

//  "xxx" in {
//    val openCL: Bit64Pointers with OpenCL10 = Factory[Bit64Pointers with OpenCL10].newInstance()
//    println(openCL.platforms)
//
//    1 should be(1)
//  }
//
//  "test" in monadic[Do] {
//    val platform = OpenCL.platforms.head
//
//    val device = platform.devices.maxBy { device =>
//      Seq(CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR).indexOf(device.deviceType)
//    }
//
//    def logger(errorInfo: String, data: ByteBuffer) = {
//      Console.err.println(errorInfo)
//    }
//    val context: OpenCL.Context = platform.createContext(logger, device).each
//
//
//
//    1 should be(1)
//  }.run.toScalaFuture
  // ???

}
