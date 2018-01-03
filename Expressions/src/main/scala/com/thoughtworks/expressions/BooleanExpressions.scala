package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait BooleanExpressions extends ValueExpressions {

  /** @template */
  type BooleanType <: ValueType

  @inject
  protected def BooleanType: Factory1[DebuggingInformation, BooleanType]

  val boolean: BooleanType = BooleanType.newInstance(debuggingInformation)
}
