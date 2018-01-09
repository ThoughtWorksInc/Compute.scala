package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableValueExpressions extends ValueExpressions {

  protected trait ValueTermApi extends super.TermApi {
    type DeltaTerm <: ValueTerm

    // FIXME: `x` should be narrow to FloatTerm
    /** Returns the symbolic difference `∂this/∂x` */
    def gradient(x: ValueTerm)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm
  }

  type ValueTerm <: (Term with Any) with ValueTermApi

//  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>
//
//    type Delta
//
//    trait TypedTermApi extends super.TypedTermApi {}
//    type TypedTerm <: ValueTerm with TypedTermApi
//  }
//
//  /** @template */
//  type ValueType <: (Type with Any) with ValueTypeApi

}
