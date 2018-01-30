package com.thoughtworks.compute.tree

import java.util.IdentityHashMap

import scala.collection.JavaConverters._
import com.thoughtworks.compute.api.Terms

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.language.higherKinds
import scala.util.hashing.{Hashing, MurmurHash3}

/**
  * @author 杨博 (Yang Bo)
  */
trait Trees extends Terms {

  final class StructuralComparisonContext extends IdentityHashMap[TreeApi, TreeApi]

  trait Operator extends TreeApi { thisOperator =>

    def structuralHashCode(context: HashCodeContext): Int = {
      val productArity: Int = this.productArity
      @tailrec
      def generateHashCode(h: Int = productPrefix.hashCode, i: Int = 0): Int = {
        if (i < productArity) {
          val childHashCode = productElement(i) match {
            case childTree: TreeApi =>
              childTree.structuralHashCode(context)
            case leaf =>
              leaf.##
          }
          generateHashCode(h = MurmurHash3.mix(h, childHashCode), i = i + 1)
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
            val productArity: Int = this.productArity
            @tailrec
            def sameFields(start: Int = 0): Boolean = {
              if (start < productArity) {
                productElement(start) match {
                  case left: TreeApi =>
                    that.productElement(start) match {
                      case right: TreeApi =>
                        if (left.isSameStructure(right, map)) {
                          sameFields(start = start + 1)
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

  trait Parameter extends TreeApi { thisParameter =>

    val id: Any

    def structuralHashCode(context: HashCodeContext): Int = {
      context.asScala.getOrElseUpdate(this, {
        val newId = context.numberOfParameters
        context.numberOfParameters = newId + 1
        newId
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

  final class AlphaConversionContext extends IdentityHashMap[TreeApi, TreeApi]

  trait TreeApi extends Product { thisTree =>
    type TermIn[C <: Category]

    def export(foreignCategory: Category, map: ExportContext): TermIn[foreignCategory.type]

    // TODO: alphaConversion

    def isSameStructure(that: TreeApi, map: StructuralComparisonContext): Boolean

    def structuralHashCode(context: HashCodeContext): Int

    def alphaConversion(context: AlphaConversionContext): TreeApi

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
