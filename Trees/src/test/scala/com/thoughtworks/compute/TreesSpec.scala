package com.thoughtworks.compute

import com.thoughtworks.compute.Trees.{AllTrees, FloatArrayTrees}
import com.thoughtworks.feature.Factory
import org.scalatest.{Assertion, FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
final class TreesSpec extends FreeSpec with Matchers {
  private val trees: AllTrees = Factory[AllTrees with Trees.StructuralTrees].newInstance()

  private def reflexive(term: => trees.Term): Assertion = {
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

  private def sameStructuralDifferentParameterName(term1: trees.Term, term2: trees.Term): Assertion = {
    term1 should be(term2)
    term1.## should be(term2.##)
  }

  private def differentStructural(term1: trees.Term, term2: trees.Term): Assertion = {
    term1 shouldNot be(term2)
    term1.## shouldNot be(term2.##)
  }

  "float" in {

    reflexive(trees.float.parameter("my_id"))
    reflexive(trees.float.literal(42.0f))

    sameStructuralDifferentParameterName(trees.float.parameter("my_id_1"), trees.float.parameter("my_id_2"))

  }

  "array" in {
    reflexive(trees.array.parameter("my_id", trees.float.literal(42.0f), Array(12, 34)))
    sameStructuralDifferentParameterName(trees.array.parameter("my_id_3", trees.float.literal(42.0f), Array(12, 34)),
                                         trees.array.parameter("my_id_4", trees.float.literal(42.0f), Array(12, 34)))
    differentStructural(
      trees.array.parameter("my_id", trees.float.literal(0.1f), Array(12, 34)),
      trees.array.parameter("my_id2", trees.float.literal(99.9f), Array(56, 78))
    )
  }

  "tuple.concatenate" - {
    "reflexive" ignore {
      reflexive(
        trees.tuple.concatenate(
          trees.float.parameter("my_id"),
          trees.float.literal(2.0f),
          trees.float.literal(3.0f)
        )
      )
    }

    "sameStructuralDifferentParameterName" ignore {
      sameStructuralDifferentParameterName(
        trees.tuple.concatenate(
          trees.float.parameter("my_id1"),
          trees.float.parameter("my_id2"),
          trees.float.literal(0.0f)
        ),
        trees.tuple.concatenate(
          trees.float.parameter("my_id2"),
          trees.float.parameter("my_id3"),
          trees.float.literal(0.0f)
        )
      )
    }

    "differentStructural" ignore {
      differentStructural(
        trees.tuple.concatenate(
          trees.float.literal(1.0f),
          trees.float.literal(0.0f)
        ),
        trees.tuple.concatenate(
          trees.float.literal(0.0f),
          trees.float.literal(1.0f)
        )
      )
    }
  }

}
