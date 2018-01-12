package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableFloats extends Floats with DifferentiableValues {
  protected trait FloatTypeApi extends super.FloatTypeApi { this: FloatType =>
    val deltaType: this.type = this

    protected trait TimesApi extends super.TimesApi { this: Times =>
      def computeDelta(context: DifferentiableContext): deltaType.TypedTerm = {
        Plus(
          Times(
            context.delta(operand0),
            operand1
          )(debuggingInformation),
          Times(
            operand0,
            context.delta(operand1)
          )(debuggingInformation)
        )(debuggingInformation)
      }
    }
    type Times <: (TypedTerm with Any) with TimesApi

    protected trait PlusApi extends super.PlusApi { this: Plus =>
      def computeDelta(context: DifferentiableContext): TypedTerm = {
        Plus(context.delta(operand0), context.delta(operand1))(debuggingInformation)
      }
    }
    type Plus <: (TypedTerm with Any) with PlusApi

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
