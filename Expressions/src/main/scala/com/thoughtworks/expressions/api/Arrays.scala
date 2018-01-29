package com.thoughtworks.expressions.api

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait Arrays extends Values {
  type Category >: this.type <: Arrays

  protected trait ValueTermApi extends super.ValueTermApi { thisValue: ValueTerm =>

    // TODO: Remove this method
    def fill: ArrayTerm {
      type Element = thisValue.ThisTerm
    }
  }

  override type ValueTerm <: (Term with Any) with ValueTermApi

  protected trait ArrayTermApi extends TermApi { thisArray: ArrayTerm =>
    type TermIn[C <: Category] = C#ArrayTerm {
      type Element = thisArray.Element#TermIn[C]
    }

    type Element <: ValueTerm

    def extract: Element

    def translate(offset: Int*): ThisTerm

  }

  type ArrayTerm <: (Term with Any) with ArrayTermApi

  @inject
  val array: Implicitly[ArrayCompanion]

  protected trait ArrayCompanionApi {

    def parameter[ElementType <: ValueType](id: Any, elementType: ElementType, shape: Int*): ArrayTerm {
      type Element = elementType.ThisTerm
    }

  }

  type ArrayCompanion <: ArrayCompanionApi

}
