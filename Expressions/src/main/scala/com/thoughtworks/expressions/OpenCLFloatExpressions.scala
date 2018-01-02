package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLFloatExpressions extends FloatExpressions with OpenCLExpressions {

  protected trait DslFloatCompanionApi extends DslTypeApi {
    override def toCode(context: Context): DslType.Code =
      DslType.Code(accessor = DslType.Accessor.Atom("float"))

  }

  protected type DslFloatCompanion <: (DslType with Any) with DslFloatCompanionApi

}
