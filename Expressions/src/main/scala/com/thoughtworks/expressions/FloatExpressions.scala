package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatExpressions extends ValueExpressions {

  /** @template */
  type FloatType <: ValueType

  @inject
  protected def FloatType: Factory.Unary[DebuggingInformation, FloatType]

  val float: FloatType = FloatType.newInstance(debuggingInformation)

}
