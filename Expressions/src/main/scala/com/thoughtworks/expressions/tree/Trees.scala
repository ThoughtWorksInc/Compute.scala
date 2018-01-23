package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.Terms
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Terms {

  protected trait TreeApi {
    type ForeignTerm[C <: Category]

    def export(foreignCategory: Category): ForeignTerm[foreignCategory.type]
  }

  protected trait TermApi extends super.TermApi { thisTree: Term =>
    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      tree.export(foreignCategory)
    }

    type Tree = TreeApi {
      type ForeignTerm[C <: Category] = thisTree.ForeignTerm[C]
    }
    protected val tree: Tree

  }

  type Term <: TermApi

}
