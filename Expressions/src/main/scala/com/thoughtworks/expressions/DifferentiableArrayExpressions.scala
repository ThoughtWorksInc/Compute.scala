package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrayExpressions extends DifferentiableValueExpressions with ArrayExpressions {

  trait ArrayBufferTermApi extends TermApi with super.ArrayBufferTermApi { outer: ArrayBufferTerm =>
    type DeltaTerm <: ArrayBufferTerm { type ElementTerm = outer.ElementTerm }
    override def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
      ???
    }
  }

  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi

  protected trait ValueTypeApi
      extends super[ArrayExpressions].ValueTypeApi
      with super[DifferentiableValueExpressions].ValueTypeApi { this: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TermApi with super.ExtractFromArrayBufferApi {

      type DeltaTerm = ExtractFromArrayBuffer

      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }
      override def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        ExtractFromArrayBuffer.newInstance(debuggingInformation, operand0.gradient(x))
      }

    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
