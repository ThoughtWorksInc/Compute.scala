package com.thoughtworks.compute.api

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Terms {
  type Category >: this.type <: Terms

  protected trait ExpressionApi {
    type TermIn[C <: Category] <: C#Term

    type ThisTerm = TermIn[Terms.this.type]

  }

  protected trait TermApi extends ExpressionApi { this: Term =>

  }

  type Term <: TermApi

  protected trait TypeApi extends ExpressionApi {}

  type Type <: TypeApi

}
