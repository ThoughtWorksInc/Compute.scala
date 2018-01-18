package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory.Factory0

/**
  * @author 杨博 (Yang Bo)
  */
trait Arrays extends Values {
  protected trait ArrayTermApi { thisArrayTerm =>
    type ElementTerm <: ValueTerm

    def extract(implicit debuggingInformationFacotry: ImplicitlyAppliedFactory[DebuggingInformation]): ElementTerm

    def translate(offset: Int*): ArrayTerm { type ElementTerm = thisArrayTerm.ElementTerm } = ???

  }

  /** @template */
  type ArrayTerm <: (Term with Any) with ArrayTermApi

  protected trait ValueTermApi extends TermApi { valueTerm: ValueTerm =>
    def filled(implicit debuggingInformationFacotry: ImplicitlyAppliedFactory[DebuggingInformation])
      : ArrayTerm { type ElementTerm = valueTerm.Self }
  }
  type ValueTerm <: (Term with Any) with ValueTermApi

}
