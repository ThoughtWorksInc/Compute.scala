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
      type DeltaTerm = LiteralTerm
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        val delta = if (x eq this) {
          1.0f
        } else {
          0.0f
        }
        LiteralTerm(delta)
      }
    }
    type Identifier <: (TypedTerm with Any) with IdentifierApi

    protected trait LiteralTermApi extends super.LiteralTermApi with TypedTermApi {
      type DeltaTerm = LiteralTerm
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        LiteralTerm(0.0f)
      }
    }
    type LiteralTerm <: (TypedTerm with Any) with LiteralTermApi

  }

  /** @template */
  type FloatType <: (ValueType with Any) with FloatTypeApi

}
