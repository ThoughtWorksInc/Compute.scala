package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayOperators extends Booleans with Arrays {

  protected trait ValueTypeApi extends super.ValueTypeApi { valueType: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TypedValueTermApi { this: ExtractFromArrayBuffer =>
      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }
    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

    @inject def ExtractFromArrayBuffer: Factory2[Implicitly[DebuggingInformation],
                                                 ArrayBufferTerm { type ElementTerm = TypedTerm },
                                                 ExtractFromArrayBuffer]

    protected trait TypedValueTermApi extends super.TypedTermApi { this: TypedTerm =>
      def filled(implicit debuggingInformation: Implicitly[DebuggingInformation])
        : ArrayFillTerm { type ElementTerm = valueType.TypedTerm } = {
        val arrayFillType = ArrayFillType[valueType.type].newInstance(valueType)
        arrayFillType.Filled.newInstance(debuggingInformation, this)
      }

    }
    type TypedTerm <: (ValueTerm with Any) with TypedValueTermApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait ArrayTypeApi extends TypeApi { this: ArrayType =>
    type ElementType <: ValueType
    val elementType: ElementType
  }

  /** @template */
  type ArrayType <: (Type with Any) with ArrayTypeApi
  protected trait ArrayFillTermApi {
    val `type`: ArrayFillType
  }

  /** @template */
  type ArrayFillTerm <: (ArrayTerm with Any) with ArrayFillTermApi

  protected trait ArrayFillTypeApi extends ArrayTypeApi { arrayFillType: ArrayFillType =>
    def name = "ArrayFill"
    val elementType: operand0.type = operand0

    protected val operand0: ElementType
    trait TypedTermApi extends super.TypedTermApi { this: TypedTerm =>
      type ElementTerm = arrayFillType.operand0.TypedTerm

      def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm = {
        ???
      }
    }

    type TypedTerm <: (ArrayFillTerm with Any) with TypedTermApi

    trait FilledApi extends TypedTermApi { this: Filled =>
      val operand0: ElementTerm
    }
    type Filled <: (TypedTerm with Any) with FilledApi

    @inject protected[ArrayOperators] def Filled: Factory2[Implicitly[DebuggingInformation], operand0.TypedTerm, Filled]

  }

  /** @template */
  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi

  @inject
  def ArrayFillType[ElementType0 <: ValueType]
    : Factory1[ElementType0, ArrayFillType { type ElementType = ElementType0 }]

  protected trait ArrayBufferTermApi {
    val `type`: ArrayBufferType
  }

  /** @template */
  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi

  protected trait ArrayBufferTypeApi extends ArrayTypeApi { thisArrayBufferType: ArrayBufferType =>
    def name = "ArrayBuffer"

    protected val operand0: ElementType
    val elementType: operand0.type = operand0

    protected val operand1: Seq[Int]
    def shape: Seq[Int] = operand1

    trait TypedTermApi extends super.TypedTermApi { this: TypedTerm =>
      type ElementTerm = thisArrayBufferType.operand0.TypedTerm

      def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm = {
        thisArrayBufferType.operand0.ExtractFromArrayBuffer.newInstance(debuggingInformation, this)
      }
    }

    type TypedTerm <: (ArrayBufferTerm with Any) with TypedTermApi

  }

  /** @template */
  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

  @inject def ArrayBufferType[ElementType0 <: ValueType]
    : Factory2[ElementType0, Seq[Int], ArrayBufferType { type ElementType = ElementType0 }]

  /** @template */
  type ArrayOffsetTerm <: ArrayTerm

  protected trait ArrayOffsetTypeApi extends ArrayTypeApi { thisArrayOffsetType: ArrayOffsetType =>

    def name = "ArrayOffset"

    protected val operand0: ArrayType { type ElementType = thisArrayOffsetType.ElementType }

    /** The offset */
    protected val operand1: Seq[Int]

    val elementType: operand0.elementType.type = operand0.elementType
    trait TypedTermApi extends super.TypedTermApi { this: TypedTerm =>
      type ElementTerm = thisArrayOffsetType.elementType.TypedTerm

      def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm = {
        ???
      }
    }

    type TypedTerm <: (ArrayOffsetTerm with Any) with TypedTermApi

  }

  /** @template */
  type ArrayOffsetType <: (ArrayType with Any) with ArrayOffsetTypeApi
  @inject def ArrayOffsetType[ElementType0 <: ValueType]: Factory2[ArrayType { type ElementType = ElementType0 },
                                                                   Seq[Int],
                                                                   ArrayOffsetType { type ElementType = ElementType0 }]

}
