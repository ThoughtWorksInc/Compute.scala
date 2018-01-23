package com.thoughtworks.expressions.api

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Terms {
  type Category >: this.type <: Terms

  protected trait ExpressionApi {
    type ForeignTerm[C <: Category] <: C#Term

    type TypedTerm = ForeignTerm[Terms.this.type]

  }

  protected trait TermApi extends ExpressionApi { this: Term =>

  }

  type Term <: TermApi

  protected trait TypeApi extends ExpressionApi {}

  type Type <: TypeApi

}
