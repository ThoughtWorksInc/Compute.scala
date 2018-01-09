package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait BooleanExpressions extends ValueExpressions {

  protected trait BooleanTypeApi extends ValueTypeApi { this: BooleanType =>
    type JvmType = Boolean
  }

  /** @template */
  type BooleanType <: (ValueType with Any) with BooleanTypeApi

  @inject protected def BooleanType: Factory1[DebuggingInformation, BooleanType]

  type BooleanTerm = boolean.TypedTerm

  val boolean: BooleanType = BooleanType.newInstance(debuggingInformation)
}
