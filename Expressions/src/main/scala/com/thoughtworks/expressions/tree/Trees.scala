package com.thoughtworks.expressions.tree

import java.util.IdentityHashMap

import com.thoughtworks.expressions.api.Terms

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Terms {

  protected trait TreeApi extends Product {
    type ForeignTerm[C <: Category]

    def export(foreignCategory: Category): ForeignTerm[foreignCategory.type]

    // TODO: parameter should have special implementation of `isSameStructure`
    def isSameStructure(that: TreeApi,
                        map: IdentityHashMap[TreeApi, TreeApi] = new IdentityHashMap[TreeApi, TreeApi]): Boolean = {
      map.get(this) match {
        case null =>
          this.getClass == that.getClass && {
            assert(this.productArity == that.productArity)
            map.put(this, that)
            this.productIterator.zip(that.productIterator).forall {
              case (left: TreeApi, right: TreeApi) =>
                left.isSameStructure(right, map)
              case (left, right) =>
                left == right
            }
          }
        case existing if existing eq that => true
        case _                            => false
      }
    }
  }

  protected trait TermApi extends super.TermApi { thisTree: Term =>
    def in(foreignCategory: Category): ForeignTerm[foreignCategory.type] = {
      tree.export(foreignCategory)
    }

    type Tree = TreeApi {
      type ForeignTerm[C <: Category] = thisTree.ForeignTerm[C]
    }
    val tree: Tree

  }

  type Term <: TermApi

}
