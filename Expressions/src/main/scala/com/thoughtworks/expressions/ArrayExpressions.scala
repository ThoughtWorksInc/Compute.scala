package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, Factory2, Factory3, inject}

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayExpressions extends BooleanExpressions {

  protected trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>

    protected trait ExtractFromArrayBufferApi extends TypedTermApi {
      protected val operand0: ArrayBufferTerm { type ElementTerm = TypedTerm }
    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

    @inject val ExtractFromArrayBuffer: Factory2[DebuggingInformation,
                                                 ArrayBufferTerm { type ElementTerm = TypedTerm },
                                                 ExtractFromArrayBuffer]

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait ArrayTermApi {
    type ElementTerm <: ValueTerm

    def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm

  }

  /** @template */
  type ArrayTerm <: (Term with Any) with ArrayTermApi

  protected trait ArrayTypeApi extends TypeApi { this: ArrayType =>
    type ElementType <: ValueType
  }

  /** @template */
  type ArrayType <: (Type with Any) with ArrayTypeApi
  protected trait ArrayFillTermApi {
    val `type`: ArrayFillType
  }

  /** @template */
  type ArrayFillTerm <: (ArrayTerm with Any) with ArrayFillTermApi

  protected trait ArrayFillTypeApi extends ArrayTypeApi { arrayFillType: ArrayFillType =>
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

    @inject protected def Filled: Factory2[Implicitly[DebuggingInformation], operand0.TypedTerm, Filled]

  }

  /** @template */
  type ArrayFillType <: (ArrayType with Any) with ArrayFillTypeApi
  @inject def ArrayFillType[ElementType0 <: ValueType]
    : Operator1[ElementType0, ArrayFillType { type ElementType = ElementType0 }]

  protected trait ArrayBufferTermApi {
    val `type`: ArrayBufferType
  }

  /** @template */
  type ArrayBufferTerm <: (ArrayTerm with Any) with ArrayBufferTermApi

  protected trait ArrayBufferTypeApi extends ArrayTypeApi {
    arrayType: ArrayBufferType =>

    protected val operand0: ElementType

    protected val operand1: Seq[Int]
    def shape: Seq[Int] = operand1

    trait TypedTermApi extends super.TypedTermApi { this: TypedTerm =>
      type ElementTerm = arrayType.operand0.TypedTerm

      def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm = {
        arrayType.operand0.ExtractFromArrayBuffer.newInstance(debuggingInformation, this)
      }
    }

    type TypedTerm <: (ArrayBufferTerm with Any) with TypedTermApi

  }

  /** @template */
  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

  /** @template */
  type ArrayViewTerm <: ArrayTerm

  protected trait ArrayViewTypeApi {
    //      FIXME: Transform 的类型应该怎么定义
    //      trait TransformApi { this: Transform =>
    //        val operand0: ArrayBufferTerm
    //        val operand1: Array[Array[Int]]
    //      }
    //
    //      type Transform <: TypedTerm with TransformApi
    //      @inject //      def Transform: Operator2[ArrayBufferTerm, Array[Array[Int]], Transform]

  }

  /** @template */
  type ArrayViewType <: (ArrayType with Any) with ArrayViewTypeApi

  @inject def ArrayBufferType[ElementType0 <: ValueType]
    : Operator2[ElementType0, Seq[Int], ArrayBufferType { type ElementType = ElementType0 }]

}
