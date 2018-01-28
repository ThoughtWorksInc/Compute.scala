package com.thoughtworks.expressions.tree

import java.util.IdentityHashMap

import com.thoughtworks.expressions.api.Terms

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Terms {

  protected trait Operator extends TreeApi {
    def isSameStructure(that: TreeApi, map: IdentityHashMap[TreeApi, TreeApi]): Boolean = {
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
        case existing =>
          existing eq that
      }
    }
  }

  protected trait Parameter extends TreeApi {

    def isSameStructure(that: TreeApi, map: IdentityHashMap[TreeApi, TreeApi]): Boolean = {
      map.get(this) match {
        case null =>
          map.put(this, that)
          true
        case existing =>
          existing eq that
      }
    }
  }

  protected trait TreeApi extends Product {
    type TermIn[C <: Category]

    def export(foreignCategory: Category, map: ExportMap): TermIn[foreignCategory.type]

    // TODO: alphaConversion

    // TODO: parameter should have special implementation of `isSameStructure`
    def isSameStructure(that: TreeApi, map: IdentityHashMap[TreeApi, TreeApi]): Boolean
  }

  type ExportMap = IdentityHashMap[TreeApi, Any]

  protected trait TermApi extends super.TermApi { thisTree: Term =>
    def in(foreignCategory: Category): TermIn[foreignCategory.type] = {
      tree.export(foreignCategory, new ExportMap)
    }

    type Tree = TreeApi {
      type TermIn[C <: Category] = thisTree.TermIn[C]
    }
    val tree: Tree

  }

  type Term <: TermApi

}
