package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLFloatExpressions extends FloatExpressions with OpenCLExpressions {

  protected trait FloatTypeApi extends DslTypeApi { this: FloatType =>
    override def toCode(context: Context): DslType.Code =
      DslType.Code(accessor = DslType.Accessor.Atom("float"))

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
