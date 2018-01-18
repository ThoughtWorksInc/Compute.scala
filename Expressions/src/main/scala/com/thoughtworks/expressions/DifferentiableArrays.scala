package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableArrays extends DifferentiableValues with ArrayOperators {

  trait ArrayBufferTermApi extends TermApi with super.ArrayBufferTermApi { outer: ArrayBufferTerm =>

//    type DeltaTerm <: ArrayBufferTerm { type ElementTerm = outer.ElementTerm }

    // TODO: `delta` should be implemented in subtypes, not here
    def computeDelta(context: DifferentiableContext): DeltaTerm = {
      ???
    }
  }

  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi

  protected trait ArrayBufferTypeApi extends super.ArrayBufferTypeApi { this: ArrayBufferType =>

    @transient lazy val deltaType = ArrayBufferType[operand0.deltaType.type].newInstance(operand0.deltaType, operand1)

    implicitly[deltaType.operand0.type =:= operand0.deltaType.type]

  }

  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

  protected trait ArrayFillTypeApi extends super.ArrayFillTypeApi with super[DifferentiableValues].TypeApi {
    arrayFillType: ArrayFillType =>

    val deltaType = ArrayFillType[operand0.deltaType.type].newInstance(operand0.deltaType)

    trait IdentifierApi extends super[ArrayFillTypeApi].TypedTermApi with super[TypeApi].TypedTermApi {
      outer: TypedTerm =>

      def computeDelta(context: DifferentiableContext): DeltaTerm = {
        deltaType.Filled.newInstance(debuggingInformation, arrayFillType.operand0.deltaType.zero(debuggingInformation))
      }

    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

    trait FilledApi extends super.FilledApi with super[ArrayFillTypeApi].TypedTermApi with super[TypeApi].TypedTermApi {
      this: Filled =>
      def computeDelta(context: DifferentiableContext): DeltaTerm = {
        deltaType.Filled.newInstance(debuggingInformation, context.delta(operand0))
      }
    }
    type Filled <: (TypedTerm with Any) with FilledApi

  }

  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi

  protected trait ArrayOffsetTypeApi extends super.ArrayOffsetTypeApi with super[DifferentiableValues].TypeApi {
    arrayOffsetType: ArrayOffsetType =>

    val deltaType = ??? // ArrayOffsetType[operand0.deltaType.type].newInstance(operand0.deltaType)

    trait IdentifierApi extends super[ArrayOffsetTypeApi].TypedTermApi with super[TypeApi].TypedTermApi {
      outer: TypedTerm =>

      def computeDelta(context: DifferentiableContext): DeltaTerm = {
        ???
      }

    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

  }

  type ArrayOffsetType <: (ArrayType with Any) with ArrayOffsetTypeApi

  protected trait ValueTypeApi
      extends super[ArrayOperators].ValueTypeApi
      with super[DifferentiableValues].ValueTypeApi { thisValueType: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TermApi with super.ExtractFromArrayBufferApi with TypedTermApi {
      this: ExtractFromArrayBuffer =>

      def computeDelta(context: DifferentiableContext): deltaType.TypedTerm = {
        val arrayBufferDelta: operand0.`type`.deltaType.TypedTerm = context.delta(operand0)
        deltaType.ExtractFromArrayBuffer.newInstance(
          debuggingInformation,
          // I am sure it is correct but I can't prove it in Scala 2
          arrayBufferDelta.asInstanceOf[ArrayBufferTerm { type ElementTerm = deltaType.TypedTerm }]
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
