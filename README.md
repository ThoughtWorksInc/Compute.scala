# Compute.scala
[![Build Status](https://travis-ci.org/ThoughtWorksInc/Compute.scala.svg)](https://travis-ci.org/ThoughtWorksInc/Compute.scala)
[![Latest version](https://index.scala-lang.org/thoughtworksinc/compute.scala/cpu/latest.svg)](https://index.scala-lang.org/thoughtworksinc/compute.scala/cpu)
[![Scaladoc](https://javadoc.io/badge/com.thoughtworks.compute/gpu_2.12.svg?label=scaladoc)](https://javadoc.io/page/com.thoughtworks.compute/gpu_2.12/latest/com/thoughtworks/compute/cpu$.html)

**Compute.scala** is a Scala library for scientific computing with N-dimensional arrays in parallel on GPU, CPU and other devices. It will be the primary back-end of the incoming [DeepLearning.scala](http://deeplearning.thoughtworks.school/) 3.0, to address performance problems we encountered in DeepLearning.scala 2.0 with [ND4J](http://ND4J.org/).

 * Compute.scala can dynamically merge multiple operators into one kernel program, which runs significantly faster when performing complex computation.
 * Compute.scala manages data buffers and other native resources in a determinate approach, consuming less memory and reducing the performance impact due to garbage collection.
 * All dimensional transformation operators (`permute`, `broadcast`, `translate`, etc) in Compute.scala are views, with no additional data buffer allocation.
 * N-dimensional arrays in Compute.scala can be split to JVM collections, which support higher-ordered functions like `map` / `reduce`, and still can run on GPU.

## Getting started

### System Requirements

Compute.scala is based on [LWJGL 3](https://www.lwjgl.org/)'s OpenCL binding, which supports AMD, NVIDIA and Intel's GPU and CPU on Linux, Windows and macOS.

Make sure you have met the following system requirements before using Compute.scala.

 * Linux, Windows or macOS
 * JDK 8
 * OpenCL runtime

The performance of Compute.scala varies with OpenCL runtimes. For best performance, install the OpenCL runtime according to the following table.

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
libraryDependencies += ("org.lwjgl" % "lwjgl" % "latest.release").jar().classifier {
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

Check [Compute.scala on Scaladex](https://index.scala-lang.org/thoughtworksinc/compute.scala) and [LWJGL customize tool](https://www.lwjgl.org/customize) for settings for Maven, Gradle and other build tools.

### Creating an N-dimensional array

Import types in `gpu` or `cpu` object according to the OpenCL runtime you want to use.

``` scala
// For N-dimensional array on GPU
import com.thoughtworks.compute.gpu._
```

``` scala
// For N-dimensional array on CPU
import com.thoughtworks.compute.cpu._
```

In Compute.scala, an N-dimensional array is typed as `Tensor`, which can be created from `Seq` or `Array`.

``` scala
val my2DArray: Tensor = Tensor(Array(Seq(1.0f, 2.0f, 3.0f), Seq(4.0f, 5.0f, 6.0f)))
```

If you print out `my2DArray`,

``` scala
println(my2DArray)
```

then the output should be

```
[[1.0,2.0,3.0],[4.0,5.0,6.0]]
```

You can also print the sizes of each dimension using the `shape` method.

``` scala
// Output 2 because my2DArray is a 2D array.
println(my2DArray.shape.length)

// Output 2 because the size of first dimension of my2DArray is 2.
println(my2DArray.shape(0)) // 2

// Output 3 because the size of second dimension of my2DArray is 3.
println(my2DArray.shape(1)) // 3
```

So `my2DArray` is a 2D array of 2x3 size.

#### Scalar value

Note that a `Tensor` can be a zero dimensional array, which is simply a scalar value.

```
val scalar = Tensor(42.f)
println(scalar.shape.length) // 0
```

### Element-wise operators

Element-wise operators are performed for each element of in `Tensor` operands.

```
val plus100 = my2DArray + Tensor.fill(100.0f, Array(2, 3))

println(plus100) // Output [[101.0,102.0,103.0],[104.0,105.0,106.0]]
```

## Design

### Lazy-evaluation

`Tensor`s in Compute.scala are immutable and lazy-evaluated. All operators that create `Tensor`s are pure, which allocate zero data buffer and not execute any time-consuming tasks. The actual computation is only performed when the final result is requested.

For example:

``` scala
val a = Tensor(Seq(Seq(1.0f, 2.0f, 3.0f), Seq(4.0f, 5.0f, 6.0f)))
val b = Tensor(Seq(Seq(7.0f, 8.0f, 9.0f), Seq(10.0f, 11.0f, 12.0f)))
val c = Tensor(Seq(Seq(13.0f, 14.0f, 15.0f), Seq(16.0f, 17.0f, 18.0f)))

val result: InlineTensor = a * b + c
```

All the `Tensor`s, including `a`, `b`, `c` and `result` are small JVM objects and no computation is performed up to now.

``` scala
println(result.toString)
```

When `result.toString` is called, the Compute.scala compiles the expression `a * b + c` into one kernel program and execute it.

Both `result` and the temporary variable `a * b` are `InlineTensor`s, indicating their computation can be inlined into a more complex kernel program. You can think of an `InlineTensor` as an `@inline def` method on device side.

This approach is faster than other libraries because we don't have to execute two kernels for multiplication and addition respectively.

Check the [Scaladoc](https://javadoc.io/page/com.thoughtworks.compute/tensors_2.12/latest/com/thoughtworks/compute/Tensors$Tensor.html) seeing which operators return `InlineTensor` or its subtype `TransformedTensor`, which can be inlined into a more complex kernel program as well.

#### Caching

By default, when `result.toString` is called more than once, the expression `a * b + c` is executed more than once.

``` scala
println(result.toString)

// The computation is performed, again
println(result.toString)
```

Fortunately, we provides a `cache` method to eagerly fill in a `NonInlineTensor`, and keep the filling data for reusing. You can convert `result` to a `NonInlineTensor`, which is associated with a non-inline kernel program (i.e. never merged into a larger kernel).

``` scala
val nonInlineTensor: NonInlineTensor = result.nonInline
val cache = nonInlineTensor.cache()

try {
  // The cache is reused. No device-side computation is performed.
  println(nonInlineTensor.toString)

  // The cache is reused. No device-side computation is performed.
  println(nonInlineTensor.toString)

  val tmp: InlineTensor = exp(nonInlineTensor)
  
  // The cache for nonInlineTensor is reused, but the exponential function is performed.
  println(tmp.toString)

  // The cache for nonInlineTensor is reused, but the exponential function is performed, again.
  println(tmp.toString)
} finally {
  cache.close()
}

// (a * b + c) is performed because cache for nonInlineTensor has been closed.
println(nonInlineTensor.toString)
```

The data buffer allocated for `nonInlineResult` is kept until `cache.close()` is invoked.

You can think of a `NonInlineTensor` as a `@noinline def` method and `cache` as a `lazy val` on device side.

By combining pure `Tensor`s along with the impure `cache` mechanism, we achieved the following goals:

* All `Tensor`s are pure. No data buffer is allocated when creating them.
* The computation of `Tensor`s can be merged together, to minimize the number of intermediate data buffers and kernel programs.
* The developers can create `cache`s for `Tensor`s, as a determinate way to manage the life-cycle of resources.

### Scala collection interoperability

#### `split`

A `Tensor` can be `split` into small `Tensor`s on the direction of a specific dimension.

For example, given a 3D tensor whose `shape` is 2×3×4,

``` scala
val my3DTensor = Tensor((0.0f until 24.0f by 1.0f).grouped(4).toSeq.grouped(3).toSeq)

val Array(2, 3, 4) = my3DTensor.shape
```

when `split` it at the dimension #0, 

``` scala
val subtensors0: Seq[Tensor] = my3DTensor.split(dimension = 0)
```

then the result should be a `Seq` of two 3×4 tensors.

``` scala
// Output: TensorSeq([[0.0,1.0,2.0,3.0],[4.0,5.0,6.0,7.0],[8.0,9.0,10.0,11.0]], [[12.0,13.0,14.0,15.0],[16.0,17.0,18.0,19.0],[20.0,21.0,22.0,23.0]])
println(subtensors0)
```

When `split` it at the dimension #1, 

``` scala
val subtensors1: Seq[Tensor] = my3DTensor.split(dimension = 1)
```

then the result should be a `Seq` of three 2×4 tensors.

``` scala
// Output: TensorSeq([[0.0,1.0,2.0,3.0],[12.0,13.0,14.0,15.0]], [[4.0,5.0,6.0,7.0],[16.0,17.0,18.0,19.0]], [[8.0,9.0,10.0,11.0],[20.0,21.0,22.0,23.0]])
println(subtensors1)
```

Then you can use arbitrary Scala collection functions on the `Seq` of subtensors.

#### `join`

Multiple `Tensor`s of the same `shape` can be merged into a larger `Tensor` via the `Tensor.join` function.

Given a `Seq` of three 2×2 `Tensor`s,

``` scala
val mySubtensors: Seq[Tensor] = Seq(
  Tensor(Seq(Seq(1.0f, 2.0f), Seq(3.0f, 4.0f))),
  Tensor(Seq(Seq(5.0f, 6.0f), Seq(7.0f, 8.0f))),
  Tensor(Seq(Seq(9.0f, 10.0f), Seq(11.0f, 12.0f))),
)
```

when `join`ing them, 
``` scala
val merged: Tensor = Tensor.join(mySubtensors)
```

then the result should be a 2x2x3 `Tensor`.

``` scala
// Output: [[[1.0,5.0,9.0],[2.0,6.0,10.0]],[[3.0,7.0,11.0],[4.0,8.0,12.0]]]
println(merged.toString)
```

Generally, when `join`ing *n* `Tensor`s of shape *a*<sub>0</sub> × *a*<sub>1</sub> × *a*<sub>2</sub> ×  ⋯ × *a*<sub>*i*</sub> , the shape of the result `Tensor` is *a*<sub>0</sub> × *a*<sub>1</sub> × *a*<sub>2</sub> ×  ⋯ × *a*<sub>*i*</sub> × *n*

#### Case study: fast matrix multiplication via `split` and `join`

By combining `split` and `join`, you can create complex computation in the following steps:

 1. Using `split` to create `Seq`s from some of dimensions of `Tensor`s.
 2. Using Scala collection functions to manipulate `Seq`s.
 3. Using `join` to merge transformed `Seq` back to `Tensor`.

For example, you can implement matrix multiplication in this style.

``` scala
def matrixMultiply1(matrix1: Tensor, matrix2: Tensor): Tensor = {
  val columns1 = matrix1.split(1)
  val columns2 = matrix2.split(1)
  val resultColumns = columns2.map { column2: Tensor =>
    (columns1 zip column2.split(0))
      .map {
        case (l: Tensor, r: Tensor) =>
          l * r.broadcast(l.shape)
      }
      .reduce[Tensor](_ + _)
  }
  Tensor.join(resultColumns)
}
```

You can imagine the Scala collection function calls as the code generator of the kernel program, thus the loop running in Scala collection will finally become an unrolled loop in the kernel program.

The above `matrixMultiply1` will create a kernel program that contains an unrolled loop of each row and column of `matrix2`. Thus it runs very fast when `matrix1` is big and `matrix2` is small. Our benchmark shows that the above `matrixMultiply1` runs even faster than ND4J's cuBLAS back-end, on a Titan X GPU, when `matrix1` is 65536×8 and `matrix2` is 8×8.

---

You can also create another version of matrix multiplication, which only unrolls the loop of each row of `matrix2`.

``` scala
def matrixMultiply2(matrix1: Tensor, matrix2: Tensor): Tensor = {
  val Array(i, j) = matrix1.shape
  val Array(`j`, k) = matrix2.shape
  val broadcastMatrix1 = matrix1.broadcast(Array(i, j, k))
  val broadcastMatrix2 = matrix2.reshape(Array(1, j, k)).broadcast(Array(i, j, k))
  val product = broadcastMatrix1 * broadcastMatrix2
  product.split(1).reduce[Tensor](_ + _)
}
```

`matrixMultiply2` will run faster than `matrixMultiply1` when `matrix1` is small.

A sophisticated matrix multiplication should dynamically switch the two implementations according to matrix size.

``` scala
val UnrollThreshold = 4000

def matrixMultiply(matrix1: Tensor, matrix2: Tensor): Tensor = {
  if (matrix1.shape.head >= UnrollThreshold) {
    matrixMultiply1(matrix1, matrix2)
  } else {
    matrixMultiply2(matrix1, matrix2)
  }
}
```

The final version of `matrixMultiply` will have good performance for both small and big matrixes.

## Benchmark

We created some benchmarks for Compute.scala and ND4J on NVIDIA and AMD GPU in an immutable style.

 * [Compute.scala vs ND4J on an NVIDIA Titan X GPU](http://jmh.morethan.io/?source=https://thoughtworksinc.github.io/Compute.scala/benchmarks/nvidia-gpu.json) ([source code](https://github.com/ThoughtWorksInc/Compute.scala/blob/nvidia-gpu/benchmarks/src/jmh/scala/com/thoughtworks/compute/benchmarks.scala))
 * [Compute.scala on an AMD RX480 GPU](http://jmh.morethan.io/?source=https://thoughtworksinc.github.io/Compute.scala/benchmarks/amd-gpu.json) ([source code](https://github.com/ThoughtWorksInc/Compute.scala/blob/amd-gpu/benchmarks/src/jmh/scala/com/thoughtworks/compute/benchmarks.scala))

Some information can be found in the benchmark result:

 * Apparently, Compute.scala supports both NVIDIA GPU and AMD GPU, while ND4J does not support AMD GPU.
 * Compute.scala is faster than ND4J when performing complex expressions.
 * Compute.scala is faster than ND4J on large arrays.
 * ND4J is faster than Compute.scala when performing one simple primary operation on small arrays.
 * ND4J's `permute` and `broadcast` are extremely slow, causing very low score in the convolution benchmark.

Note that the above result of ND4J is not the same as the performance in Deeplearning4j, because Deeplearning4j uses ND4J in a mutable style (i.e. `a *= b; a += c` instead of `a * b + c`) and ND4J has some undocumented optimizions for `permute` and `broadcast` when they are invoked with some special parameters from Deeplearning4j.

## Future work

Now this project is only a minimum viable product. Many important features are still under development:

* Support tensors of elements other than single-precision floating-point ([#104](https://github.com/ThoughtWorksInc/Compute.scala/issues/104)).
* Add more OpenCL math functions ([#101](https://github.com/ThoughtWorksInc/Compute.scala/issues/101)).
* Further optimization of performance ([#62, #103](https://github.com/ThoughtWorksInc/Compute.scala/labels/performance)).
* Other back-ends (CUDA, Vulkan Compute).

Contribution is welcome. Check [good first issues](https://github.com/ThoughtWorksInc/Compute.scala/labels/good%20first%20issue) to start hacking.
