package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueExpressions extends Expressions {

  /** @template */
  type ValueType <: Type

}
