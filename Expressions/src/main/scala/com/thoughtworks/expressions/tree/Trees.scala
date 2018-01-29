package com.thoughtworks.expressions.tree

import java.util.IdentityHashMap

import scala.collection.JavaConverters._
import com.thoughtworks.expressions.api.Terms

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.util.hashing.{Hashing, MurmurHash3}

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Terms {

  final class StructuralComparisonContext extends IdentityHashMap[TreeApi, TreeApi]

  protected trait Operator extends TreeApi {

    def structuralHashCode(context: HashCodeContext): Int = {
      @tailrec
      def generateHashCode(productArity: Int = this.productArity, h: Int = productPrefix.hashCode, i: Int = 0): Int = {
        if (i < productArity) {
          val childHashCode = productElement(i) match {
            case childTree: TreeApi =>
              childTree.structuralHashCode(context)
            case leaf =>
              leaf.##
          }
          generateHashCode(productArity = productArity, h = MurmurHash3.mix(h, childHashCode), i = i + 1)
        } else {
          MurmurHash3.finalizeHash(h, productArity)
        }
      }
      context.asScala.getOrElseUpdate(this, generateHashCode())
    }

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean = {
      map.get(this) match {
        case null =>
          this.getClass == that.getClass && {
            assert(this.productArity == that.productArity)
            map.put(this, that)
            @tailrec
            def sameFields(productArity: Int = this.productArity, start: Int = 0): Boolean = {
              if (start < productArity) {
                productElement(start) match {
                  case left: TreeApi =>
                    that.productElement(start) match {
                      case right: TreeApi =>
                        if (left.isSameStructure(right, map)) {
                          sameFields(productArity = productArity, start = start + 1)
                        } else {
                          false
                        }
                      case _ =>
                        false
                    }
                  case _ =>
                    false
                }
              } else {
                true
              }
            }
            sameFields()
          }
        case existing =>
          existing eq that
      }
    }
  }

  protected trait Parameter extends TreeApi {

    def structuralHashCode(context: HashCodeContext): Int = {
      context.asScala.getOrElseUpdate(this, {
        val id = context.numberOfParameters
        context.numberOfParameters = id + 1
        id
      })
    }

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean = {
      map.get(this) match {
        case null =>
          map.put(this, that)
          true
        case existing =>
          existing eq that
      }
    }
  }

  final class HashCodeContext extends IdentityHashMap[TreeApi, Int] {
    var numberOfParameters = 0
  }

  protected trait TreeApi extends Product {
    type TermIn[C <: Category]

    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type]

    // TODO: alphaConversion

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean

    def structuralHashCode(context: HashCodeContext): Int

  }

  final class ExportContext extends IdentityHashMap[TreeApi, Any]

  protected trait TermApi extends super.TermApi { thisTree: Term =>
    def in(foreignCategory: Category): TermIn[foreignCategory.type] = {
      tree.export(foreignCategory, new ExportContext)
    }

    type Tree = TreeApi {
      type TermIn[C <: Category] = thisTree.TermIn[C]
    }
    val tree: Tree

  }

  type Term <: TermApi

}
