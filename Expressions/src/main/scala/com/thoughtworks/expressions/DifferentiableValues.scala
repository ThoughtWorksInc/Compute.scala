package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableValues extends DifferentiableExpressions with Values {

  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>

    val deltaType: ValueType

    def zero(implicit debuggingInformation: Implicitly[DebuggingInformation]): TypedTerm

//    protected trait TypedTermApi extends TermApi with super.TypedTermApi {
//      type DeltaTerm <: TypedTerm
//    }
//    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

    protected trait ZeroDeltaApi extends TypedTermApi { this: TypedTerm =>

      def computeDelta(context: DifferentiableContext): DeltaTerm = {
        deltaType.zero(debuggingInformation)
      }
    }

    type Identifier <: (TypedTerm with Any) with ZeroDeltaApi

    type Literal <: (TypedTerm with Any) with LiteralApi with ZeroDeltaApi

  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

}
