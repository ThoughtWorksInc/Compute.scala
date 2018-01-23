package com.thoughtworks.expressions.api
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Values extends Terms {
  type Category >: this.type <: Values

  protected trait ValueExpressionApi extends ExpressionApi {
    type ForeignTerm[C <: Category] <: C#ValueTerm
  }

  protected trait ValueApi extends TermApi with ValueExpressionApi { this: ValueTerm =>
  }

  /** @template */
  type ValueTerm <: (Term with Any) with ValueApi

  protected trait ValueTypeApi extends ValueExpressionApi{

    type JvmValue

//    type TypedTerm <: ValueTerm

    def literal(value: JvmValue): TypedTerm

    def parameter(id: Any): TypedTerm

  }

  type ValueType <: ValueTypeApi
}
