package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableBooleans extends Booleans with DifferentiableValues {
  protected trait BooleanTypeApi extends ValueTypeApi with super.BooleanTypeApi { this: BooleanType =>
    val deltaType: this.type = this
  }

  type BooleanType <: (ValueType with Any) with BooleanTypeApi
}
