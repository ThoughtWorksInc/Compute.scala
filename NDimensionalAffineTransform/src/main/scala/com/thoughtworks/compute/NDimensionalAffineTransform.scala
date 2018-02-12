package com.thoughtworks.compute

import scala.annotation.tailrec

/**
  * @author 杨博 (Yang Bo)
  */
object NDimensionalAffineTransform {

  type MatrixData = Array[Double]
  type VectorData = Array[Double]

  def translate(offsets: VectorData): MatrixData = {
    val length = offsets.length
    val matrix = Array.ofDim[Double](length * (length + 1))
    @tailrec
    def loop(i: Int): Unit = {
      if (i < length) {
        matrix(i * (length + 1) + i) = 1.0
        matrix(i * (length + 1) + length) = offsets(i)
        loop(i + 1)
      }
    }
    loop(0)
    matrix
  }

  @inline
  def preConcatenate(matrix01: MatrixData, matrix12: MatrixData, length0: Int): MatrixData = {
    val length1 = matrix01.length / (length0 + 1)
    val length2 = matrix12.length / (length1 + 1)
    val matrix02 = Array.ofDim[Double]((length0 + 1) * length2)
    zip(matrix01, matrix12, matrix02, length0, length1, length2)
    matrix02
  }

  @inline
  def concatenate(matrix12: MatrixData, matrix01: MatrixData, length2: Int): MatrixData = {

    val length1 = matrix12.length / length2 - 1
    val length0 = matrix01.length / length1 - 1

    val matrix02 = Array.ofDim[Double]((length0 + 1) * length2)
    zip(matrix01, matrix12, matrix02, length0, length1, length2)
    matrix02
  }

  private def zip(matrix01: MatrixData,
                          matrix12: MatrixData,
                          matrix02: MatrixData,
                          length0: Int,
                          length1: Int,
                          length2: Int): Unit = {

    @tailrec
    def loop2(index2: Int): Unit = {
      if (index2 < length2) {
        @tailrec
        def loop0(index0: Int): Unit = {
          if (index0 < length0) {
            @tailrec
            def loop1(index1: Int, accumulator: Double): Double = {
              if (index1 < length1) {
                loop1(index1 + 1,
                      accumulator +
                        matrix12(index2 * (length1 + 1) + index1) *
                          matrix01(index1 * (length0 + 1) + index0))
              } else {
                accumulator
              }
            }
            matrix02(index2 * (length0 + 1) + index0) = loop1(0, 0.0)

            loop0(index0 + 1)
          }
        }
        loop0(0)
        @tailrec
        def loop1(index1: Int, accumulator: Double): Double = {
          if (index1 < length1) {
            loop1(index1 + 1,
                  accumulator +
                    matrix12(index2 * (length1 + 1) + index1) *
                      matrix01(index1 * (length0 + 1) + length0))
          } else {
            accumulator
          }
        }
        matrix02(index2 * (length0 + 1) + length0) = loop1(0, matrix12(index2 * (length1 + 1) + length1))
        loop2(index2 + 1)
      }
    }
    loop2(0)

  }

  def transform(matrix: MatrixData, source: VectorData): VectorData = {
    val sourceLength = source.length
    val destination = Array.ofDim[Double](matrix.length / (sourceLength + 1))
    transform(matrix, source, destination)
    destination
  }

  private def transform(matrix: MatrixData, source: VectorData, destination: VectorData): Unit = {
    val sourceLength = source.length
    val destinationLength = destination.length
    if (matrix.length != (sourceLength + 1) * destinationLength) {
      throw new IllegalArgumentException
    }
    @tailrec
    def rowLoop(y: Int): Unit = {
      if (y < destinationLength) {
        @tailrec
        def columnLoop(x: Int, accumulator: Double): Double = {
          if (x < sourceLength) {
            columnLoop(x + 1, accumulator + matrix(y * (sourceLength + 1) + x) * source(x))
          } else {
            accumulator
          }
        }
        destination(y) = columnLoop(0, matrix(y * (sourceLength + 1) + sourceLength))

        rowLoop(y + 1)
      }
    }
    rowLoop(0)
  }

}
