package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatExpressions <: Expressions {

  type DslFloat = DslFloat.DslExpression

  /** @template */
  protected type DslFloatCompanion <: DslType

  @inject
  protected def DslFloatCompanion: Factory.Unary[DebuggingInformation, DslFloatCompanion]

  val DslFloat: DslFloatCompanion = DslFloatCompanion.newInstance(debuggingInformation)

}
