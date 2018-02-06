package com.thoughtworks.compute

import com.thoughtworks.compute.Trees.FloatPointerTrees
import com.thoughtworks.feature.Factory
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
final class TreesSpec extends FreeSpec with Matchers {

  "hashCode" in {

    val trees: FloatPointerTrees = Factory[Trees.FloatPointerTrees with Trees.StructuralTrees].newInstance()
    def reflexive(term: => trees.Term) = {
      val t0 = term
      val t1 = term
      t0 should be(t0)
      t0.## should be(t0.##)
      t1 should be(t1)
      t1.## should be(t1.##)
      t0 should be(t1)
      t0.## should be(t1.##)

      sameStructuralDifferentParameterName(t0, t0.alphaConversion)
    }

    def sameStructuralDifferentParameterName(term1: trees.Term, term2: trees.Term) = {
      term1 should be(term2)
      term1.## should be(term2.##)
    }

    def differentStructural(term1: trees.Term, term2: trees.Term) = {
      term1 shouldNot be(term2)
      term1.## shouldNot be(term2.##)
    }

    reflexive(trees.float.parameter("my_id"))
    reflexive(trees.float.literal(42.0f))
    reflexive(trees.pointer.parameter("my_id", trees.float, 42.0f, Array(12, 34)))

    sameStructuralDifferentParameterName(trees.float.parameter("my_id_1"), trees.float.parameter("my_id_2"))
    sameStructuralDifferentParameterName(trees.pointer.parameter("my_id_3", trees.float, 42.0f, Array(12, 34)),
                                         trees.pointer.parameter("my_id_4", trees.float, 42.0f, Array(12, 34)))

    differentStructural(
      trees.pointer.parameter("my_id", trees.float, 0.1f, Array(12, 34)),
      trees.pointer.parameter("my_id2", trees.float, 99.9f, Array(56, 78))
    )
  }

}
