package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait BooleanExpressions extends ValueExpressions {

  /** @template */
  type BooleanType <: ValueType

  @inject
  protected def BooleanType: Factory.Unary[DebuggingInformation, BooleanType]

  val boolean: BooleanType = BooleanType.newInstance(debuggingInformation)
}
