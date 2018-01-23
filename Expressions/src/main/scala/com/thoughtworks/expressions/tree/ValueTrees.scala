package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Values
import com.thoughtworks.feature.Factory.Factory1

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueTrees extends Values with Trees {

  protected trait ValueTree extends Tree with ValueApi { thisValue: ValueTerm =>

    def factory: Factory1[ExporterApi {
                            type ForeignTerm[C <: Category] = thisValue.ForeignTerm[C]
                          },
                          Self]
  }

  override type ValueTerm <: (Term with Any) with ValueTree

}
