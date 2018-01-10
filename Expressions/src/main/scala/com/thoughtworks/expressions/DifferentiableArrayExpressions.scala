package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrayExpressions extends DifferentiableValueExpressions with ArrayExpressions {

  trait ArrayBufferTermApi extends TermApi with super.ArrayBufferTermApi { outer: ArrayBufferTerm =>
    type DeltaTerm <: ArrayBufferTerm { type ElementTerm = outer.ElementTerm }

    // TODO: `gradient` should be implemented in subtypes, not here
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
      type DeltaTerm = ArrayFillTerm { type ElementTerm = arrayFillType.operand0.TypedTerm }

      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        arrayFillType.operand0.zero.filled
      }

    }

    type TypedTerm <: (ArrayFillTerm with Any) with TypedTermApi

  }

  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi

  protected trait ValueTypeApi
      extends super[ArrayExpressions].ValueTypeApi
      with super[DifferentiableValueExpressions].ValueTypeApi { this: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TermApi with super.ExtractFromArrayBufferApi {
      this: ExtractFromArrayBuffer =>

      type DeltaTerm = ExtractFromArrayBuffer

      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }
      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        ExtractFromArrayBuffer.newInstance(debuggingInformation, operand0.gradient(x))
      }

    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

    protected trait TypedTermApi extends super.TypedTermApi with TypedValueTermApi { this: TypedTerm =>
    }

    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
