package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrayExpressions extends ArrayExpressions with DifferentiableBooleanExpressions {
  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>

    protected trait ExtractFromArrayBufferApi extends ValueTermApi with super.ExtractFromArrayBufferApi {
      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }
      def gradient(x: ValueTerm)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        ???
      }

    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
