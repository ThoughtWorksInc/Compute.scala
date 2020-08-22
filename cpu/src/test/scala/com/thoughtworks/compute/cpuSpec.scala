package com.thoughtworks.compute

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author 杨博 (Yang Bo)
  */
class cpuSpec extends AnyFreeSpec with Matchers {
  "computational graph" in {
    import cpu._
    val a = Tensor.fill(2.0f, Array(2, 3)).nonInline
    val b = Tensor.fill(2.0f, Array(2, 3)).nonInline
    val c = (a + b).nonInline
    val d = (c + b).nonInline
    d.toString should be("[[6.0,6.0,6.0],[6.0,6.0,6.0]]")
  }

  "join" in {
    import cpu._
    val a = Tensor.fill(42.0f, Array(3, 4))
    val b = Tensor.fill(43.0f, Array(3, 4))

    val t0 = Tensor.join(Seq(a, b), 0)
    t0.shape should be(Array(2, 3, 4))
    t0.toString should be(
      "[[[42.0,42.0,42.0,42.0],[42.0,42.0,42.0,42.0],[42.0,42.0,42.0,42.0]],[[43.0,43.0,43.0,43.0],[43.0,43.0,43.0,43.0],[43.0,43.0,43.0,43.0]]]")

    val t1 = Tensor.join(Seq(a, b), 1)
    t1.shape should be(Array(3, 2, 4))
    t1.toString should be(
      "[[[42.0,42.0,42.0,42.0],[43.0,43.0,43.0,43.0]],[[42.0,42.0,42.0,42.0],[43.0,43.0,43.0,43.0]],[[42.0,42.0,42.0,42.0],[43.0,43.0,43.0,43.0]]]")

    val t2 = Tensor.join(Seq(a, b), 2)
    t2.shape should be(Array(3, 4, 2))
    t2.toString should be(
      "[[[42.0,43.0],[42.0,43.0],[42.0,43.0],[42.0,43.0]],[[42.0,43.0],[42.0,43.0],[42.0,43.0],[42.0,43.0]],[[42.0,43.0],[42.0,43.0],[42.0,43.0],[42.0,43.0]]]")

  }
}
