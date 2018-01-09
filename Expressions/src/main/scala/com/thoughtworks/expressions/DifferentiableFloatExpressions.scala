package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableFloatExpressions extends DifferentiableValueExpressions with FloatExpressions {

  protected trait FloatTypeApi extends super.FloatTypeApi with super.ValueTypeApi { this: FloatType =>
    protected trait TypedTermApi extends super.TypedTermApi with ValueTermApi {
      type DeltaTerm <: TypedTerm
    }
    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

    protected trait IdentifierApi extends TypedTermApi {
      type DeltaTerm = Literal
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        val delta = if (x eq this) {
          1.0f
        } else {
          0.0f
        }
        Literal(delta)
      }
    }
    type Identifier <: (TypedTerm with Any) with IdentifierApi

    protected trait LiteralApi extends super.LiteralApi with TypedTermApi {
      type DeltaTerm = Literal
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        Literal(0.0f)
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi

  }

  /** @template */
  type FloatType <: (ValueType with Any) with FloatTypeApi

}
