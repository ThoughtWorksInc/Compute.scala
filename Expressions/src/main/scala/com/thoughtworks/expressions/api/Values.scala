package com.thoughtworks.expressions.api
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Values extends Terms {
  type Category >: this.type <: Values

  protected trait ValueExpressionApi extends ExpressionApi {
    type TermIn[C <: Category] <: C#ValueTerm
    type TypeIn[C <: Category] <: C#ValueType
    type ThisType = TypeIn[Values.this.type]
  }

  protected trait ValueTermApi extends TermApi with ValueExpressionApi { this: ValueTerm =>
  }

  /** @template */
  type ValueTerm <: (Term with Any) with ValueTermApi

  protected trait ValueTypeApi extends ValueExpressionApi {

    type JvmValue

    def literal(value: JvmValue): ThisTerm

    def parameter(id: Any): ThisTerm

  }

  type ValueType <: (Type with Any) with ValueTypeApi
}
