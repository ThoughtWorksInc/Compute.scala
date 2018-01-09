package com.thoughtworks.expressions

import com.thoughtworks.expressions.OpenCLExpressions._

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLBooleanExpressions extends OpenCLExpressions with BooleanExpressions {

  protected trait BooleanTypeApi extends super.BooleanTypeApi { this: BooleanType =>
    override def toCode(context: OpenCLContext): OpenCLType.Code =
      OpenCLType.Code(accessor = OpenCLType.Accessor.Atom("bool"))

  }

  type BooleanType <: (ValueType with Any) with BooleanTypeApi

}
