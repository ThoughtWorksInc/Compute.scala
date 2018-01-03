package com.thoughtworks.expressions

import shapeless.Nat
import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLPointerExpressions extends PointerExpressions with OpenCLBooleanExpressions {

  protected trait DslTypeApi
      extends super[PointerExpressions].DslTypeApi
      with super[OpenCLBooleanExpressions].DslTypeApi {
    this: DslType =>

  }

  type DslType <: (Expression with Any) with DslTypeApi
  trait ValueTypeApi extends DslTypeApi with super.ValueTypeApi { elementType: ValueType =>

    protected trait PointerTypeApi[NumberOfDimensions <: Nat]
        extends super.PointerTypeApi[NumberOfDimensions]
        with DslTypeApi { pointerType: PointerType[NumberOfDimensions] =>

      override def toCode(context: Context): DslType.Code = ???

      protected trait TypedTermApi
          extends super[DslTypeApi].TypedTermApi
          with super[PointerTypeApi].TypedTermApi {
        this: TypedTerm =>
      }

      type TypedTerm <: (Term with Any) with TypedTermApi

      protected trait DereferenceApi extends super.DereferenceApi with elementType.TypedTermApi {
        this: elementType.TypedTerm =>
        def toCode(context: Context): Term.Code = ???
      }

      type Dereference <: (elementType.TypedTerm with Any) with DereferenceApi

    }

    type PointerType[NumberOfDimensions <: Nat] <: (DslType with Any) with PointerTypeApi[NumberOfDimensions]

  }
  type ValueType <: (DslType with Any) with ValueTypeApi

}
