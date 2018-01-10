package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueExpressions extends Expressions {

  protected trait ValueTypeApi extends TypeApi { this: ValueType =>
    type JvmType
    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

    protected trait LiteralApi extends TypedTermApi {
      protected val operand0: JvmType
    }
    type Literal <: (TypedTerm with Any) with LiteralApi
    @inject def Literal: Operator1[JvmType, Literal]
  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

  /** @template */
  type ValueTerm <: Term
}
