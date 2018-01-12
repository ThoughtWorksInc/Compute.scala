package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait Arrays extends Values {
  protected trait ArrayTermApi {
    type ElementTerm <: ValueTerm

    def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm

  }

  /** @template */
  type ArrayTerm <: (Term with Any) with ArrayTermApi

  protected trait ValueTermApi extends TermApi { valueTerm: ValueTerm =>
    def filled(implicit debuggingInformation: Implicitly[DebuggingInformation])
      : ArrayTerm { type ElementTerm = valueTerm.Self }
  }
  type ValueTerm <: (Term with Any) with ValueTermApi

}
