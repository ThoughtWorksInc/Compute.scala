package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrayExpressions extends DifferentiableValueExpressions with ArrayExpressions {

  trait ArrayBufferTermApi extends TermApi with super.ArrayBufferTermApi { outer: ArrayBufferTerm =>

//    type DeltaTerm <: ArrayBufferTerm { type ElementTerm = outer.ElementTerm }

    // TODO: `gradient` should be implemented in subtypes, not here
    def gradient(context: DifferentiableExpressions.Context)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
      ???
    }
  }

  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi

  protected trait ArrayBufferTypeApi extends super.ArrayBufferTypeApi { this: ArrayBufferType =>

    @transient lazy val deltaType = ArrayBufferType[operand0.deltaType.type].newInstance(operand0.deltaType, operand1)

    implicitly[deltaType.operand0.type =:= operand0.deltaType.type]

  }

  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

  protected trait ArrayFillTypeApi extends super.ArrayFillTypeApi with super[DifferentiableValueExpressions].TypeApi {
    arrayFillType: ArrayFillType =>

    val deltaType = ArrayFillType[operand0.deltaType.type].newInstance(operand0.deltaType)

    trait IdentifierApi extends super[ArrayFillTypeApi].TypedTermApi with super[TypeApi].TypedTermApi {
      outer: TypedTerm =>

      def gradient(context: DifferentiableExpressions.Context)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        deltaType.Filled.newInstance(debuggingInformation, arrayFillType.operand0.deltaType.zero)
      }

    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

    trait FilledApi extends super.FilledApi with super[ArrayFillTypeApi].TypedTermApi with super[TypeApi].TypedTermApi {
      this: Filled =>
      def gradient(context: DifferentiableExpressions.Context)(implicit debuggingInformation: Implicitly[DebuggingInformation]): DeltaTerm = {
        deltaType.Filled.newInstance(debuggingInformation, operand0.gradient(context))
      }
    }
    type Filled <: (TypedTerm with Any) with FilledApi

  }

  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi

  protected trait ValueTypeApi
      extends super[ArrayExpressions].ValueTypeApi
      with super[DifferentiableValueExpressions].ValueTypeApi { thisValueType: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TermApi with super.ExtractFromArrayBufferApi with TypedTermApi {
      this: ExtractFromArrayBuffer =>

      def gradient(context: DifferentiableExpressions.Context)(implicit debuggingInformation: Implicitly[DebuggingInformation]): deltaType.TypedTerm = {
        val arrayBufferGradient: operand0.`type`.deltaType.TypedTerm = operand0.gradient(context)
        deltaType.ExtractFromArrayBuffer.newInstance(
          debuggingInformation,
          // I am sure it is correct but I can't prove it in Scala 2
          arrayBufferGradient.asInstanceOf[ArrayBufferTerm { type ElementTerm = deltaType.TypedTerm }]
        )
      }

    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

    protected trait TypedTermApi extends super.TypedTermApi with TypedValueTermApi { this: TypedTerm =>
    }

    type TypedTerm <: (ValueTerm with Any) with TypedTermApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
