package com.thoughtworks.expressions

import org.scalatest._
import com.thoughtworks.feature.Factory
import shapeless.nat._

/**
  * @author 杨博 (Yang Bo)
  */
class ExpressionsSpec extends FreeSpec with Matchers {

  "fill" in {

    val hyperparameters: BuiltIns { type DebuggingInformation = Debugging.Name } = {
      Factory[BuiltIns].newInstance()
    }

    import hyperparameters._

    val x: float.Identifier = float.Identifier()
    val fillShader = ShaderDefinition("fill", Seq(x), x)
    val sourceCode = generateSourceCode(fillShader).mkString
    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  "map" in {

    val hyperparameters = {
      Factory[BuiltIns].newInstance()
    }

    import hyperparameters._
    val x: float.pointer3d.Identifier = float.pointer3d.Identifier()

  }

  // TODO: Don't use GetGlobalId, use TheNet instead
  // TODO: Use ShaderDefinition instead of ShaderDefinition
  // ShaderDefinition("my_kernel", Seq(Parameter(x, DslFloat)), Seq(DslEffect(DslEffect.Update(x))))

  //  hyperparameters.Dsl

}
