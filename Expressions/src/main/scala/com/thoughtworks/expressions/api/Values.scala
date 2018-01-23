package com.thoughtworks.expressions.api
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Values extends Terms {
  type Category >: this.type <: Values

  protected trait ValueApi extends TermApi { this: ValueTerm =>
    type ForeignTerm[C <: Category] <: C#ValueTerm
  }

  /** @template */
  type ValueTerm <: (Term with Any) with ValueApi

  protected trait ValueCompanionApi {

    type JvmValue

    type CompanionTerm <: ValueTerm

    def literal(value: JvmValue): CompanionTerm

  }

}
