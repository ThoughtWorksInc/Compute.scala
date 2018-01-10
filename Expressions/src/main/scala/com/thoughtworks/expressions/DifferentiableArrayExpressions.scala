package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrayExpressions extends DifferentiableValueExpressions with ArrayExpressions {

  trait ArrayBufferTermApi extends TermApi with super.ArrayBufferTermApi { outer: ArrayBufferTerm =>
//    type DeltaTerm <: ArrayBufferTerm { type ElementTerm = outer.ElementTerm }

    // TODO: `gradient` should be implemented in subtypes, not here
    def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
      ???
    }
  }

  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi
  protected trait ArrayBufferTypeApi extends super.ArrayBufferTypeApi { this: ArrayBufferType =>

    @transient lazy val deltaType = ArrayBufferType[operand0.deltaType.type].newInstance(operand0.deltaType, operand1)

  }

  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

//  trait ArrayFillTermApi extends super.ArrayFillTermApi { outer: ArrayFillTerm =>
//    type DeltaTerm <: ArrayFillTerm { type ElementTerm = outer.ElementTerm }
//  }
//
//  type ArrayFillTerm <: (ArrayTerm with Any) with ArrayFillTermApi

  protected trait ArrayFillTypeApi extends super.ArrayFillTypeApi with super[DifferentiableValueExpressions].TypeApi {
    arrayFillType: ArrayFillType =>

    val deltaType = ArrayFillType[operand0.deltaType.type].newInstance(operand0.deltaType)

    trait TypedTermApi extends super[ArrayFillTypeApi].TypedTermApi with super[TypeApi].TypedTermApi {
      outer: TypedTerm =>

      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        deltaType.Filled.newInstance(debuggingInformation, arrayFillType.operand0.deltaType.zero)
      }

    }

    type TypedTerm <: (ArrayFillTerm with Any) with TypedTermApi

  }

  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi

  protected trait ValueTypeApi
      extends super[ArrayExpressions].ValueTypeApi
      with super[DifferentiableValueExpressions].ValueTypeApi { this: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TermApi with super.ExtractFromArrayBufferApi with TypedTermApi {
      this: ExtractFromArrayBuffer =>

//      type DeltaTerm = ExtractFromArrayBuffer

      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }

      def gradient(x: Term)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
//        val gradient0: operand0.DeltaTerm = operand0.gradient(x)
//        operand0.`type`.deltaType.operand0.ExtractFromArrayBuffer.newInstance(debuggingInformation, gradient0)

        ???
      }

    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

    protected trait TypedTermApi extends super.TypedTermApi with TypedValueTermApi { this: TypedTerm =>
    }

    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
