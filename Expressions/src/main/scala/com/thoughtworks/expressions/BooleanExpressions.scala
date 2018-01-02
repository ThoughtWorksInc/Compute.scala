package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait BooleanExpressions <: Expressions {

  type DslBoolean = DslBoolean.DslExpression

  /** @template */
  protected type DslBooleanCompanion <: DslType

  @inject
  protected def DslBooleanCompanion: Factory.Unary[DebuggingInformation,DslBooleanCompanion]

  val DslBoolean: DslBooleanCompanion = DslBooleanCompanion.newInstance(debuggingInformation)
}
