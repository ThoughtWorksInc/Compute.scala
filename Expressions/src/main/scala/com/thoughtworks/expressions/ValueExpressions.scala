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

    protected trait LiteralTermApi extends TypedTermApi {
      protected val operand0: JvmType
    }
    type LiteralTerm <: (TypedTerm with Any) with LiteralTermApi
    @inject
    def LiteralTerm: Operator1[JvmType, LiteralTerm]
  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

  type ValueTerm <: Term
}
