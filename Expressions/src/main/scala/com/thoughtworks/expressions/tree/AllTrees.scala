package com.thoughtworks.expressions.tree

import com.thoughtworks.expressions.api.{Arrays, Floats}

/**
  * @author 杨博 (Yang Bo)
  */
trait AllTrees extends ArrayTrees with FloatTrees {
  type Category >: this.type <: Arrays with Floats
}
