package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring.Implicits._

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLFloatExpressions extends FloatExpressions with OpenCLExpressions {

  protected trait FloatTypeApi extends super.FloatTypeApi with super[OpenCLExpressions].ValueTypeApi {
    this: FloatType =>
    override def toCode(context: Context): Type.Code =
      Type.Code(accessor = Type.Accessor.Atom("float"))

    protected trait LiteralApi extends super[ValueTypeApi].LiteralApi {
      override def toCode(context: Context): Term.Code = {
        Term.Code(accessor = Term.Accessor.Atom(fast"${operand0.toString}f"))
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
