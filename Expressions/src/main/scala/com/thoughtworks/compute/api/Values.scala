package com.thoughtworks.compute.api
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Values extends Terms {
  type Category >: this.type <: Values

  protected trait ValueExpressionApi extends ExpressionApi { thisValue =>

    type JvmValue
    type TermIn[C <: Category] <: C#ValueTerm
    type TypeIn[C <: Category] <: C#ValueType {
      type JvmValue = thisValue.JvmValue
    }
    type ThisType = TypeIn[Values.this.type]
  }

  protected trait ValueTermApi extends TermApi with ValueExpressionApi { this: ValueTerm =>
  }

  /** @template */
  type ValueTerm <: (Term with Any) with ValueTermApi

  protected trait ValueTypeApi extends ValueExpressionApi {

    def literal(value: JvmValue): ThisTerm

    def parameter(id: Any): ThisTerm

  }

  type ValueType <: (Type with Any) with ValueTypeApi
}
