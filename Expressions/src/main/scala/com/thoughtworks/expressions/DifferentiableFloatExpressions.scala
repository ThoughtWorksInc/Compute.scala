package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableFloatExpressions extends FloatExpressions with DifferentiableValueExpressions {
  protected trait FloatTypeApi extends super.FloatTypeApi { this: FloatType =>
    val deltaType: this.type = this
  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
