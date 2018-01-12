package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory0, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait Floats extends Values {

  protected trait FloatTypeApi extends ValueTypeApi { this: FloatType =>
    type JvmType = Float

    def name = "Float"

    def zero(implicit debuggingInformation: Implicitly[DebuggingInformation]): Literal = Literal(0.0f)

    protected trait FloatTypedTermApi extends super.TypedTermApi { this: TypedTerm =>
      def +(that: TypedTerm)(implicit debuggingInformation: Implicitly[DebuggingInformation]): TypedTerm = {
        Plus(this, that)
      }
      def *(that: TypedTerm)(implicit debuggingInformation: Implicitly[DebuggingInformation]): TypedTerm = {
        Times(this, that)
      }
    }

    type TypedTerm <: (ValueTerm with Any) with FloatTypedTermApi

    protected trait TimesApi {
      val operand0: TypedTerm
      val operand1: TypedTerm
    }

    type Times <: (TypedTerm with Any) with TimesApi

    @inject def Times: Operator2[TypedTerm, TypedTerm, Times]

    protected trait PlusApi {
      val operand0: TypedTerm
      val operand1: TypedTerm
    }

    type Plus <: (TypedTerm with Any) with PlusApi

    @inject def Plus: Operator2[TypedTerm, TypedTerm, Plus]

  }

  /** @template */
  type FloatType <: (ValueType with Any) with FloatTypeApi

  @inject protected def FloatType: Factory0[FloatType]

  val float: FloatType = FloatType.newInstance()

  type FloatTerm = float.TypedTerm

}
