package com.thoughtworks.expressions.api

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait Arrays extends Values {

  protected trait ValueApi extends super.ValueApi { thisValue: ValueTerm =>
    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.ThisTerm
    }
  }

  override type ValueTerm <: (Term with Any) with ValueApi

  protected trait ArrayApi extends TermApi { thisArray: ArrayTerm =>
    type Element <: ValueTerm

    def shape: Array[Int]

    def extract: Element

  }

  type ArrayTerm <: (Term with Any) with ArrayApi

  @inject
  val ArrayTerm: Implicitly[ArrayCompanion]

  protected trait ArrayCompanionApi {

    def parameter[Element0 <: ValueTerm](id: Any, elementType: ValueType { type ThisTerm = Element0 }, shape: Int*)
      : ArrayTerm {
        type Element = Element0
      }

  }

  type ArrayCompanion <: ArrayCompanionApi

}
