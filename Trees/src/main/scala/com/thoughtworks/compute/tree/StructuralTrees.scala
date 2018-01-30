package com.thoughtworks.compute.tree

import java.util.IdentityHashMap

import scala.collection.mutable.ArrayBuffer

/**
  * @author 杨博 (Yang Bo)
  */
trait StructuralTrees extends Trees {

  protected trait TermApi extends super.TermApi { this: Term =>
    override def hashCode(): Int = {
      tree.structuralHashCode(new HashCodeContext)
    }

    override def equals(that: Any): Boolean = {
      that match {
        case that: TermApi =>
          tree.isSameStructure(that.tree, new StructuralComparisonContext)
        case _ =>
          false
      }
    }
  }

  type Term <: TermApi
}
