package com.thoughtworks.compute

import com.thoughtworks.compute.Trees.FloatArrayTrees
import com.thoughtworks.feature.Factory
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
final class TreesSpec extends FreeSpec with Matchers {

  "hashCode" in {

    val trees: FloatArrayTrees = Factory[Trees.FloatArrayTrees with Trees.StructuralTrees].newInstance()

    trees.float.literal(42.0f).## should be(trees.float.literal(42.0f).##)
    trees.float.literal(42.0f).## shouldNot be(trees.float.literal(41.0f).##)
    trees.float.parameter("my_id").## should be(trees.float.parameter("my_id").##)
    trees.float.parameter("my_id_1").## should be(trees.float.parameter("my_id_2").##)

    trees.array.parameter("my_id", trees.float, 42.0f, Array(12, 34)).## should be(
      trees.array.parameter("my_id2", trees.float, 42.0f, Array(12, 34)).##)
    
    trees.array.parameter("my_id", trees.float, 0.1f, Array(12, 34)).## shouldNot be(
      trees.array.parameter("my_id2", trees.float, 99.9f, Array(56, 78)).##)

  }

}
