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

}
