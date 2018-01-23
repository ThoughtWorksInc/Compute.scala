package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Values
import com.thoughtworks.feature.Factory.Factory1

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueTrees extends Values with Trees {

  protected trait ValueTypeApi extends super.ValueTypeApi {

    def factory: Factory1[TreeApi {
      type ForeignTerm[C <: Category] = TypedTerm#ForeignTerm[C]
    },
      TypedTerm]

  }

  type ValueType <: ValueTypeApi

  protected trait ValueApi extends TermApi with super.ValueApi { thisValue: ValueTerm =>

    def factory: Factory1[TreeApi {
                            type ForeignTerm[C <: Category] = thisValue.ForeignTerm[C]
                          },
                          TypedTerm]
  }

  override type ValueTerm <: (Term with Any) with ValueApi

}
