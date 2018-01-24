package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Values
import com.thoughtworks.feature.Factory.Factory1

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueTrees extends Values with Trees {

  protected trait ValueTypeApi extends super.ValueTypeApi {
    def in(foreignCategory: Category): TypeIn[foreignCategory.type]

    def factory: Factory1[TreeApi { type TermIn[C <: Category] = ThisTerm#TermIn[C] }, ThisTerm]

  }

  type ValueType <: ValueTypeApi

  protected trait ValueApi extends TermApi with super.ValueApi { thisValue: ValueTerm =>

    def factory: Factory1[TreeApi { type TermIn[C <: Category] = thisValue.TermIn[C] }, ThisTerm]
  }

  type ValueTerm <: (Term with Any) with ValueApi

}
