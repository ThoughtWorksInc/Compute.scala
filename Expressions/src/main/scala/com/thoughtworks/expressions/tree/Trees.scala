package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Terms
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Terms {

  protected trait ExporterApi {
    type ForeignTerm[C <: Category]

    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type]
  }

  protected trait Tree extends TermApi { thisTree: Term =>
    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type]

    type Exporter = ExporterApi {
      type ForeignTerm[C <: Category] = thisTree.ForeignTerm[C]
    }
    protected val exporter: Exporter

  }

  type Term <: TermApi with Tree

}
