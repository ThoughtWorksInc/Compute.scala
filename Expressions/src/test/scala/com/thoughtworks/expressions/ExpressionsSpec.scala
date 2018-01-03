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
    val shader = ShaderDefinition("fill", Seq(x), x)
    val sourceCode = generateSourceCode(shader).mkString
    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  "id" ignore {

    val hyperparameters: BuiltIns { type DebuggingInformation = Debugging.Name } = {
      Factory[BuiltIns].newInstance()
    }

    import hyperparameters._

//    Factory[PointerType[FloatType, _3]]

    val x: float.pointer3d.Identifier = float.pointer3d.Identifier()

    val shader = ShaderDefinition("id", Seq(x), float.Dereference(x))
    val sourceCode = generateSourceCode(shader).mkString

    println(sourceCode) // FIXME: replace println to a scalatest assertion

  }

  // TODO: Don't use GetGlobalId, use TheNet instead
  // TODO: Use ShaderDefinition instead of ShaderDefinition
  // ShaderDefinition("my_kernel", Seq(Parameter(x, DslFloat)), Seq(DslEffect(DslEffect.Update(x))))

  //  hyperparameters.Dsl

}
