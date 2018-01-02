package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory

/**
  * @author 杨博 (Yang Bo)
  */
class ExpressionsSpec {

  val hyperparameters: BuiltIns { type DebuggingInformation = AnyRef } = {
    Factory[BuiltIns].newInstance()
  }

  import hyperparameters._

  val x = DslFloat.Identifier()

  // TODO: Don't use GetGlobalId, use TheNet instead
  // TODO: Use ShaderDefinition instead of KernelDefinition
  // KernelDefinition("my_kernel", Seq(Parameter(x, DslFloat)), Seq(DslEffect(DslEffect.Update(x))))

  //  hyperparameters.Dsl

}
