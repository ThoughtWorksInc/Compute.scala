package com.thoughtworks.compute.api

import com.thoughtworks.compute.Anonymous.Implicitly
import com.thoughtworks.feature.Factory.inject
import org.apache.commons.math3.linear.RealMatrix

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

    @deprecated(since = "0.2.0", message = "Use [[transform]] instead.")
    def translate(offset: Int*): ThisTerm

    def transform(matrix: RealMatrix): ThisTerm
  }

  type ArrayTerm <: (Term with Any) with ArrayTermApi

  @inject
  val array: Implicitly[ArrayCompanion]

  protected trait ArrayCompanionApi {

    def parameter[Padding, ElementType <: ValueType { type JvmValue = Padding }](id: Any,
                                                                                 elementType: ElementType,
                                                                                 padding: ElementType#JvmValue,
                                                                                 shape: Int*): ArrayTerm {
      type Element = elementType.ThisTerm
    }

  }

  type ArrayCompanion <: ArrayCompanionApi

}
