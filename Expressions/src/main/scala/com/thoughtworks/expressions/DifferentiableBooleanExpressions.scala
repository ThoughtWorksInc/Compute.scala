package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableBooleanExpressions extends DifferentiableValueExpressions with BooleanExpressions {

  protected trait BooleanTypeApi extends super.BooleanTypeApi with super.ValueTypeApi { this: BooleanType =>
    protected trait TypedTermApi extends super.TypedTermApi with ValueTermApi {
      type DeltaTerm <: TypedTerm
    }
    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

    protected trait IdentifierApi extends TypedTermApi {
      type DeltaTerm = LiteralTerm
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        val delta = if (x eq this) {
          true
        } else {
          false
        }
        LiteralTerm(delta)
      }
    }
    type Identifier <: (TypedTerm with Any) with IdentifierApi

    protected trait LiteralTermApi extends super.LiteralTermApi with TypedTermApi {
      type DeltaTerm = LiteralTerm
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {

        LiteralTerm(false)
      }
    }
    type LiteralTerm <: (TypedTerm with Any) with LiteralTermApi

  }

  /** @template */
  type BooleanType <: (ValueType with Any) with BooleanTypeApi

}
