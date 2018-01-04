package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLFloatExpressions extends FloatExpressions with OpenCLExpressions {

  protected trait FloatTypeApi extends super.FloatTypeApi { this: FloatType =>
    override def toCode(context: Context): Type.Code =
      Type.Code(accessor = Type.Accessor.Atom("float"))

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
