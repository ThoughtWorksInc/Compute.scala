package com.thoughtworks.compute

import java.awt.geom.AffineTransform

import org.scalatest._

import scala.util.Random
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author 杨博 (Yang Bo)
  */
final class NDimensionalAffineTransformSpec extends AnyFreeSpec with Matchers {

  def arrayToAffineTransform(matrix: Array[Double]): AffineTransform = {
    matrix match {
      case Array(m00, m01, m02, m10, m11, m12) =>
        new AffineTransform(m00, m10, m01, m11, m02, m12)
      case _ =>
        throw new IllegalArgumentException
    }
  }

  private def checkConcatenate2D(matrix0: Array[Double], matrix1: Array[Double]) = {
    locally {
      val at = arrayToAffineTransform(matrix0)
      at.preConcatenate(arrayToAffineTransform(matrix1))

      val expected = arrayToAffineTransform(NDimensionalAffineTransform.preConcatenate(matrix0, matrix1, 2))
      at should be(expected)
    }

    locally {
      val at = arrayToAffineTransform(matrix1)
      at.concatenate(arrayToAffineTransform(matrix0))
      val expected = arrayToAffineTransform(NDimensionalAffineTransform.concatenate(matrix1, matrix0, 2))
      at should be(expected)
    }
  }

  "zip 2D" in {
    checkConcatenate2D(
      Array(
        1.0, 0.0, 3.5, //
        0.0, 1.0, 4.2
      ),
      Array(
        3.0, 0.0, 0.0, //
        0.0, 2.0, 0.0
      )
    )

    checkConcatenate2D(
      Array.fill(6)(Random.nextInt(100).toDouble),
      Array.fill(6)(Random.nextInt(100).toDouble)
    )
  }

}
