package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableValueExpressions extends DifferentiableExpressions with ValueExpressions {

  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>
    def zero(implicit debuggingInformation: Implicitly[DebuggingInformation]): TypedTerm

    protected trait TypedTermApi extends TermApi with super.TypedTermApi {
      type DeltaTerm <: TypedTerm
    }
    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

    protected trait ZeroGradientApi extends TypedTermApi {
      type DeltaTerm = TypedTerm
      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        zero
      }
    }

    type Identifier <: (TypedTerm with Any) with ZeroGradientApi

    type Literal <: (TypedTerm with Any) with LiteralApi with ZeroGradientApi

  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

}
