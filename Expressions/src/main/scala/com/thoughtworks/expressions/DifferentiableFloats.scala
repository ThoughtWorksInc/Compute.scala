package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory.Factory0
/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableFloats extends Floats with DifferentiableValues {
  protected trait FloatTypeApi extends super.FloatTypeApi { this: FloatType =>
    val deltaType: this.type = this

    protected trait TimesApi extends super.TimesApi { this: Times =>
      def computeDelta(context: DifferentiableContext): deltaType.TypedTerm = {
        implicit val debuggingInformationFactory = ImplicitlyAppliedFactory.make(debuggingInformation)
        Plus(
          Times(
            context.delta(operand0),
            operand1
          )(debuggingInformationFactory),
          Times(
            operand0,
            context.delta(operand1)
          )(debuggingInformationFactory)
        )(debuggingInformationFactory)
      }
    }
    type Times <: (TypedTerm with Any) with TimesApi

    protected trait PlusApi extends super.PlusApi { this: Plus =>
      def computeDelta(context: DifferentiableContext): TypedTerm = {
        implicit val debuggingInformationFactory = ImplicitlyAppliedFactory.make(debuggingInformation)
        Plus(context.delta(operand0), context.delta(operand1))(debuggingInformationFactory)
      }
    }
    type Plus <: (TypedTerm with Any) with PlusApi

  }

  type FloatType <: (ValueType with Any) with FloatTypeApi

}
