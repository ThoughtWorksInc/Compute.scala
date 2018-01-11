package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import com.thoughtworks.expressions.OpenCLExpressions._

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLFloatExpressions extends FloatExpressions with OpenCLExpressions {

  protected trait FloatTypeApi extends super.FloatTypeApi with super[OpenCLExpressions].ValueTypeApi {
    this: FloatType =>
    def toCode(context: OpenCLContext): OpenCLType.Code =
      OpenCLType.Code(accessor = OpenCLType.Accessor.Atom("float"))

    protected trait FloatTypedTermApi extends super.FloatTypedTermApi with TypedTermApi { this: TypedTerm =>
    }

    type TypedTerm <: (ValueTerm with Any) with FloatTypedTermApi

    protected trait LiteralApi extends super[ValueTypeApi].LiteralApi { this: Literal =>
      override def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        OpenCLTerm.Code(accessor = OpenCLTerm.Accessor.Atom(fast"${operand0.toString}f"))
      }
    }
    type Literal <: (TypedTerm with Any) with LiteralApi

    protected trait TimesApi extends super.TimesApi { this: Times =>
      override def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        val typeReference = context.get(`type`)
        val packedType = typeReference.packed
        OpenCLTerm.Code(
          localDefinitions =
            fastraw"""
              const $packedType $id = ${context.get(operand0).packed} * ${context.get(operand1).packed};
            """,
          accessor = OpenCLTerm.Accessor.Packed(Fastring(id), typeReference.unpacked.length)
        )
      }
    }
    type Times <: (TypedTerm with Any) with TimesApi

    protected trait PlusApi extends super.PlusApi { this: Plus =>
      override def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        val typeReference = context.get(`type`)
        val packedType = typeReference.packed
        OpenCLTerm.Code(
          localDefinitions =
            fastraw"""
              const $packedType $id = ${context.get(operand0).packed} + ${context.get(operand1).packed};
            """,
          accessor = OpenCLTerm.Accessor.Packed(Fastring(id), typeReference.unpacked.length)
        )
      }
    }
    type Plus <: (TypedTerm with Any) with PlusApi

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
