package com.thoughtworks.expressions.api

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Terms {
  type Category >: this.type <: Terms

  protected trait TermApi { this: Term =>
    type ForeignTerm[C <: Category] <: C#Term

    type Self = ForeignTerm[Terms.this.type]

  }

  type Term <: TermApi

}
