package com.thoughtworks.compute

import org.scalatest.{AsyncFreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class cpuSpec extends AsyncFreeSpec with Matchers {
  "computational graph" in {
    import cpu._
    val a = Tensor.fill(2.0f, Array(2, 3)).nonInline
    val b = Tensor.fill(2.0f, Array(2, 3)).nonInline
    val c = (a + b).nonInline
    val d = (c + b).nonInline
    d.toString should be("[[6.0,6.0,6.0],[6.0,6.0,6.0]]")
  }
}
