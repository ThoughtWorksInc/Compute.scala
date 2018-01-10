package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrayExpressions extends DifferentiableValueExpressions with ArrayExpressions {

  trait ArrayBufferTermApi extends TermApi with super.ArrayBufferTermApi { outer: ArrayBufferTerm =>
    type DeltaTerm <: ArrayBufferTerm { type ElementTerm = outer.ElementTerm }
    def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
      ???
    }
  }

  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi

  trait ArrayFillTermApi extends super.ArrayFillTermApi { outer: ArrayFillTerm =>
    type DeltaTerm <: ArrayFillTerm { type ElementTerm = outer.ElementTerm }
  }

  type ArrayFillTerm <: (ArrayTerm with Any) with ArrayFillTermApi

  protected trait ArrayFillTypeApi extends super.ArrayFillTypeApi {
    arrayFillType: ArrayFillType =>

    trait TypedTermApi extends super.TypedTermApi { outer: TypedTerm =>
      type DeltaTerm = Filled

//      def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm = {
//        ???
//      }

      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        val zero = arrayFillType.operand0.zero // FIXME: should invoke elementTerm.gradient instead of zero
        Filled.newInstance(debuggingInformation, zero)
      }

    }

    type TypedTerm <: (ArrayFillTerm with Any) with TypedTermApi

  }

  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi

  protected trait ValueTypeApi
      extends super[ArrayExpressions].ValueTypeApi
      with super[DifferentiableValueExpressions].ValueTypeApi { this: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TermApi with super.ExtractFromArrayBufferApi {

      type DeltaTerm = ExtractFromArrayBuffer

      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }
      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        ExtractFromArrayBuffer.newInstance(debuggingInformation, operand0.gradient(x))
      }

    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
