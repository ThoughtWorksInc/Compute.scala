package com.thoughtworks.expressions

import com.thoughtworks.expressions.OpenCLValues._

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLBooleans extends OpenCLValues with Booleans {

  protected trait BooleanTypeApi extends super.BooleanTypeApi { this: BooleanType =>
    def toCode(context: OpenCLContext): OpenCLType.Code =
      OpenCLType.Code(accessor = OpenCLType.Accessor.Atom("bool"))

  }

  type BooleanType <: (ValueType with Any) with BooleanTypeApi

}
