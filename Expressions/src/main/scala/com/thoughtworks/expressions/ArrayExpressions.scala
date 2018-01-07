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

    protected trait ExtractApi extends TypedTermApi {
      val operand0: ArrayTerm { type ElementTerm = TypedTerm }
    }

    type Extract <: (TypedTerm with Any) with ExtractApi

    @inject
    def Extract: Operator1[ArrayTerm { type ElementTerm = TypedTerm }, Extract]
  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait ArrayTypeApi extends TypeApi {
    arrayType: ArrayType =>

    type ElementType <: ValueType

    val operand0: ElementType
    val operand1: Seq[Int]

    def shape: Seq[Int] = operand1

    trait TypedTermApi extends super.TypedTermApi with ArrayTermApi { this: TypedTerm =>
      type ElementTerm = arrayType.operand0.TypedTerm

      def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm = {
        arrayType.operand0.Extract(this)
      }
    }

    type TypedTerm <: (ArrayTerm with Any) with TypedTermApi
    //      FIXME: Transform 的类型应该怎么定义
//      trait TransformApi { this: Transform =>
//        val operand0: ArrayTerm
//        val operand1: Array[Array[Int]]
//      }
//
//      type Transform <: TypedTerm with TransformApi
//      @inject
//      def Transform: Operator2[ArrayTerm, Array[Array[Int]], Transform]

  }

  trait ArrayTermApi { this: ArrayTerm =>

    val `type`: ArrayType

//    protected def asTypedTerm: `type`.TypedTerm

    type ElementTerm <: ValueTerm

    def isOutOfBound: BooleanTerm = ???

    def extract(implicit debuggingInformation: Implicitly[DebuggingInformation]): ElementTerm

  }

  type ArrayTerm <: (Term with Any) with ArrayTermApi

  /** @template */
  type ArrayType <: (Type with Any) with ArrayTypeApi

  @inject
  def ArrayType[ElementType0 <: ValueType]
    : Operator2[ElementType0, Seq[Int], ArrayType { type ElementType = ElementType0 }]

}
