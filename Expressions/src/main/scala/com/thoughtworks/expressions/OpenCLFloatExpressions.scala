package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.expressions.OpenCLExpressions._

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLFloatExpressions extends FloatExpressions with OpenCLExpressions {

  protected trait FloatTypeApi extends super.FloatTypeApi with super[OpenCLExpressions].ValueTypeApi {
    this: FloatType =>
    override def toCode(context: OpenCLContext): OpenCLType.Code =
      OpenCLType.Code(accessor = OpenCLType.Accessor.Atom("float"))

    protected trait LiteralApi extends super[ValueTypeApi].LiteralApi {
      override def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        OpenCLTerm.Code(accessor = OpenCLTerm.Accessor.Atom(fast"${operand0.toString}f"))
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
