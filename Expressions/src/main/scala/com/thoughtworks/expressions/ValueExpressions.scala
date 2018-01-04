package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueExpressions extends Expressions {

  trait ValueTypeApi extends TypeApi { this: ValueType =>
    type JvmType
  }

  /** @template */
  type ValueType <: (Type with Any) with ValueTypeApi

}
