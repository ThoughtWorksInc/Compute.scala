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
      type DeltaTerm = Literal
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        val delta = if (x eq this) {
          true
        } else {
          false
        }
        Literal(delta)
      }
    }
    type Identifier <: (TypedTerm with Any) with IdentifierApi

    protected trait LiteralApi extends super.LiteralApi with TypedTermApi {
      type DeltaTerm = Literal
      def gradient(x: ValueTerm)(
          implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {

        Literal(false)
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi

  }

  /** @template */
  type BooleanType <: (ValueType with Any) with BooleanTypeApi

}
