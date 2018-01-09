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
    val sourceCode = generateOpenCLKernelSourceCode("fill", Seq(x), Seq(x)).mkString
    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  "id" in {

    val hyperparameters: AllOpenCLExpressions { type DebuggingInformation = Debugging.Name } =
      Factory[AllOpenCLExpressions].newInstance()

    import hyperparameters._

    val floatArray3d = ArrayBufferType(float, Seq(32, 32, 32))
    val x: floatArray3d.Identifier = floatArray3d.Identifier()

    val sourceCode = generateOpenCLKernelSourceCode("id", Seq(x), Seq(x.extract)).mkString

    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  "differentiable id" in {

    val hyperparameters
      : AllOpenCLExpressions with AllDifferentiableExpressions { type DebuggingInformation = Debugging.Name } =
      Factory[AllOpenCLExpressions with AllDifferentiableExpressions].newInstance()

    import hyperparameters._

    val floatArray3d = ArrayBufferType(float, Seq(32, 32, 32))
    val x: floatArray3d.Identifier = floatArray3d.Identifier()

    val sourceCode = generateOpenCLKernelSourceCode("id", Seq(x), Seq(x.extract)).mkString

    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

}
