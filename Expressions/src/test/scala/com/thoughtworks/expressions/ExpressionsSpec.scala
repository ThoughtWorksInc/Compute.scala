package com.thoughtworks.expressions

import com.thoughtworks.expressions.Builtins.{AllDifferentiableExpressions, AllOpenCLExpressions}
import org.scalatest._
import com.thoughtworks.feature.Factory
import OpenCLExpressions.generateOpenCLKernelSourceCode

/**
  * @author 杨博 (Yang Bo)
  */
class ExpressionsSpec extends FreeSpec with Matchers {

  "fill" in {

    val hyperparameters: AllOpenCLExpressions { type DebuggingInformation = Debugging.Name } = {
      Factory[AllOpenCLExpressions].newInstance()
    }

    import hyperparameters._

    val x: float.Identifier = float.Identifier()
    val sourceCode = generateOpenCLKernelSourceCode("fill", 3, Seq(x), Seq(float.Literal(42.0f))).mkString
    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  "id" in {

    val hyperparameters: AllOpenCLExpressions { type DebuggingInformation = Debugging.Name } =
      Factory[AllOpenCLExpressions].newInstance()

    import hyperparameters._

    val dimentions = Seq(32, 32, 32)
    val floatArray3d = ArrayBufferType.newInstance(float, dimentions)
    val x: floatArray3d.Identifier = floatArray3d.Identifier()

    val sourceCode = generateOpenCLKernelSourceCode("id", dimentions.length, Seq(x), Seq(x.extract)).mkString

    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  "differentiable id" in {

    val hyperparameters =
      Factory[AllOpenCLExpressions with AllDifferentiableExpressions].newInstance()

    import hyperparameters._

    val dimensions = Seq(32, 32, 32)
    val floatArray3d = ArrayBufferType.newInstance(float, dimensions)
    val x: floatArray3d.Identifier = floatArray3d.Identifier()

    val deltaX: floatArray3d.Identifier = floatArray3d.Identifier()

    //    x.extract.

    val f = x.extract
    val deltaOfId = delta(f, x -> deltaX)

    val sourceCode =
      generateOpenCLKernelSourceCode("id_backward", dimensions.length, Seq(x, deltaX), Seq(deltaOfId)).mkString

    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

}
