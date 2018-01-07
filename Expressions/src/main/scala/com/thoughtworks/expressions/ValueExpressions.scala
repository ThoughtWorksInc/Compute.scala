package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueExpressions extends Expressions {

  protected trait ValueTypeApi extends TypeApi { this: ValueType =>
    type JvmType
    type TypedTerm <: ValueTerm with TypedTermApi
  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

  type ValueTerm <: Term
}
