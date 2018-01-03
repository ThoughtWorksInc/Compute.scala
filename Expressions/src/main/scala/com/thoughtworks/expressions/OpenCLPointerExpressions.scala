package com.thoughtworks.expressions

import shapeless.Nat
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLPointerExpressions extends PointerExpressions with OpenCLBooleanExpressions {

//  protected trait PointerTypeApi extends super.PointerTypeApi { this: PointerType =>
//    def toCode(context: Context): DslType.Code = ???
//  }
//
//  type PointerType <: (DslType with Any) with   PointerTypeApi

  protected trait PointerTypeApi[+ElementType <: DslType, NumberOfDimensions <: Nat]
      extends DslTypeApi
      with super.PointerTypeApi[ElementType, NumberOfDimensions] { this: PointerType[ElementType, NumberOfDimensions] =>
    override def toCode(context: Context): DslType.Code = ???

    protected trait DslExpressionApi
        extends super[DslTypeApi].DslExpressionApi
        with super[PointerTypeApi].DslExpressionApi {}

    type DslExpression <: (OpenCLPointerExpressions.this.DslExpression with Any) with DslExpressionApi

  }

  type PointerType[+ElementType <: DslType, NumberOfDimensions <: Nat] <: (DslType with Any) with PointerTypeApi[
    ElementType,
    NumberOfDimensions]

  protected trait DslTypeApi
      extends super[PointerExpressions].DslTypeApi
      with super[OpenCLBooleanExpressions].DslTypeApi {
    this: DslType =>

  }

  type DslType <: (Expression with Any) with DslTypeApi

}
