package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableExpressions extends Expressions {

  protected trait TermApi extends super.TermApi {
    type DeltaTerm <: Term

    // FIXME: `x` should be narrow to FloatTerm or change to Context
    /** Returns the symbolic difference `∂this/∂x` */
    def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm
  }

  type Term <: (Expression with Any) with TermApi

  protected trait TypeApi extends super.TypeApi { this: Type =>

    val deltaType: Type

    protected trait TypedTermApi extends TermApi with super.TypedTermApi { this: TypedTerm =>
      type DeltaTerm = deltaType.TypedTerm
    }

    type TypedTerm <: (Term with Any) with TypedTermApi

  }

  type Type <: (Expression with Any) with TypeApi

}
