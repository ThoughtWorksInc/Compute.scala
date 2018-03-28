# Compute.scala

**Compute.scala** is a Scala library for scientific computing on N-dimensional arrays in parallel on GPU, CPU and other devices. It will be the primary back-end of the incoming [DeepLearning.scala](http://deeplearning.thoughtworks.school/) 3.0, to address performance problems we encountered in DeepLearning.scala 2.0 with [nd4j](http://nd4j.org/).

 * Compute.scala can dynamically merge multiple operators into one kernel program, which runs significantly faster when performing complex computation.
 * Compute.scala manages memory and other native resource in a determinate approach, consuming less memory.
 * All dimensional transformation operators (`permute`, `broadcast`, `reshape`, etc) in Compute.scala are views, with no additional buffer allocation.
 * N-dimensional arrays in Compute.scala can be converted from / to JVM collection, which support higher-ordered functions like `map` / `reduce`, and still can run on GPU.

## Getting started

TODO:

## Benchmark

 * [Compute.scala vs Nd4j on NVIDIA GPU](http://jmh.morethan.io/?source=https://thoughtworksinc.github.io/Compute.scala/benchmarks/nvidia-gpu.json)
 * [Compute.scala on AMD GPU](http://jmh.morethan.io/?source=https://thoughtworksinc.github.io/Compute.scala/benchmarks/amd-gpu.json)

## Supported platforms

Compute.scala is based on OpenCL, supporting AMD, NVIDIA and Intel's GPU and CPU on Linux, Windows and macOS.
