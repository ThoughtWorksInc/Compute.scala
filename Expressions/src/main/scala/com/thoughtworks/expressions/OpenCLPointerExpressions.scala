package com.thoughtworks.expressions

import shapeless.Nat
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLPointerExpressions extends PointerExpressions with OpenCLBooleanExpressions {

  protected trait PointerTypeApi[+ElementType <: ValueType, NumberOfDimensions <: Nat]
      extends DslTypeApi
      with super.PointerTypeApi[ElementType, NumberOfDimensions] { this: PointerType[ElementType, NumberOfDimensions] =>

    override def toCode(context: Context): DslType.Code = ???

    protected trait TermApi extends super.TermApi with super.TypedTermApi { this: TypedTerm =>
    }

    type TypedTerm <: (PointerTerm[elementType.type, NumberOfDimensions] with Any) with TermApi

  }

  type PointerType[+ElementType <: ValueType, NumberOfDimensions <: Nat] <: (DslType with Any) with PointerTypeApi[
    ElementType,
    NumberOfDimensions]

  protected trait DslTypeApi
      extends super[PointerExpressions].DslTypeApi
      with super[OpenCLBooleanExpressions].DslTypeApi {
    this: DslType =>

  }

  type DslType <: (Expression with Any) with DslTypeApi
  trait ValueTypeApi extends super.ValueTypeApi { this: ValueType =>

    protected trait DereferenceApi[NumberOfDimensions <: Nat] extends TermApi {
      this: Dereference[NumberOfDimensions] =>
      val operand0: PointerTerm[ValueType, NumberOfDimensions]
      def toCode(context: Context): Term.Code = ???
    }

    type Dereference[NumberOfDimensions <: Nat] <: (TypedTerm with Any) with DereferenceApi[NumberOfDimensions]
  }
  type ValueType <: (DslType with Any) with ValueTypeApi

}
