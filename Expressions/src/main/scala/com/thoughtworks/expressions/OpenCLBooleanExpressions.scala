package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLBooleanExpressions extends OpenCLExpressions with BooleanExpressions {

  protected trait BooleanTypeApi extends DslTypeApi { this: ValueType =>
    override def toCode(context: Context): DslType.Code =
      DslType.Code(accessor = DslType.Accessor.Atom("bool"))

  }

  type BooleanType <: (ValueType with Any) with BooleanTypeApi

}
