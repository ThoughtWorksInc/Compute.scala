package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLBooleanExpressions extends OpenCLExpressions with BooleanExpressions {

  protected trait DslBooleanCompanionApi extends DslTypeApi { this: DslType =>
    override def toCode(context: Context): DslType.Code =
      DslType.Code(accessor = DslType.Accessor.Atom("bool"))

  }

  protected type DslBooleanCompanion <: (DslType with Any) with DslBooleanCompanionApi

}
