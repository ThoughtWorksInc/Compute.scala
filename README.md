# Compute.scala

**Compute.scala** is a Scala library for scientific computing with N-dimensional arrays in parallel on GPU, CPU and other devices. It will be the primary back-end of the incoming [DeepLearning.scala](http://deeplearning.thoughtworks.school/) 3.0, to address performance problems we encountered in DeepLearning.scala 2.0 with [nd4j](http://nd4j.org/).

 * Compute.scala can dynamically merge multiple operators into one kernel program, which runs significantly faster when performing complex computation.
 * Compute.scala manages memory and other native resource in a determinate approach, consuming less memory.
 * All dimensional transformation operators (`permute`, `broadcast`, `reshape`, etc) in Compute.scala are views, with no additional buffer allocation.
 * N-dimensional arrays in Compute.scala can be converted from / to JVM collection, which support higher-ordered functions like `map` / `reduce`, and still can run on GPU.

## Getting started

### System Requirements

Compute.scala is based on [LWJGL 3](https://www.lwjgl.org/)'s OpenCL binding, which supports AMD, NVIDIA and Intel's GPU and CPU on Linux, Windows and macOS.

Make sure you have met the following system requirements before using Compute.scala.

 * Linux, Windows or macOS
 * JDK 8
 * OpenCL runtime

The performance of Compute.scala varies according to which OpenCL runtime you are using. For best performance, install OpenCL runtime according to the following table.

| | Linux  | Windows | macOS |
| --- | --- | --- | --- |
| NVIDIA GPU  | [NVIDIA GPU Driver](http://www.nvidia.com/drivers) | [NVIDIA GPU Driver](http://www.nvidia.com/drivers) | macOS's built-in OpenCL SDK |
| AMD GPU | [AMDGPU-PRO Driver](https://support.amd.com/en-us/kb-articles/Pages/AMDGPU-PRO-Driver-for-Linux-Release-Notes.aspx) | [AMD OpenCL™ 2.0 Driver](https://support.amd.com/en-us/kb-articles/Pages/OpenCL2-Driver.aspx) | macOS's built-in OpenCL SDK |
| Intel or AMD CPU | [POCL](http://portablecl.org/) | [POCL](http://portablecl.org/) | [POCL](http://portablecl.org/) |

Especially, Compute.scala produces non-vectorized code, which needs POCL's auto-vectorization feature for best performance when running on CPU.

### Project setup

The artifacts of Compute.scala is published on Maven central repository for Scala 2.11 and 2.12. Add the following settings to your `build.sbt` if you are using [sbt](https://www.scala-sbt.org/).

``` sbt
libraryDependencies += "com.thoughtworks.compute" %% "cpu" % "latest.release"
libraryDependencies += "com.thoughtworks.compute" %% "gpu" % "latest.release"

// Platform dependent runtime of LWJGL core library
libraryDependencies += ("org.lwjgl" % "lwjgl" % "latest.release" % Runtime).jar().classifier {
  import scala.util.Properties._
  if (isMac) {
    "natives-macos"
  } else if (isLinux) {
    "natives-linux"
  } else if (isWin) {
    "natives-windows"
  } else {
    throw new MessageOnlyException(s"lwjgl does not support $osName")
  }
}
```

Check [Compute.scala on Scaladex](https://index.scala-lang.org/thoughtworksinc/compute.scala) and [LWJGL customize tool](https://www.lwjgl.org/customize) for settings for other build tools.

## Benchmark

 * [Compute.scala vs Nd4j on NVIDIA GPU](http://jmh.morethan.io/?source=https://thoughtworksinc.github.io/Compute.scala/benchmarks/nvidia-gpu.json)
 * [Compute.scala on AMD GPU](http://jmh.morethan.io/?source=https://thoughtworksinc.github.io/Compute.scala/benchmarks/amd-gpu.json)
