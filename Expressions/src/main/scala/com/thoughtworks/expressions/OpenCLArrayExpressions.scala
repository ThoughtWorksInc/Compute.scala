package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import shapeless.{Nat, Sized, Succ}

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLArrayExpressions extends ArrayExpressions with OpenCLBooleanExpressions {

  protected trait TypeApi extends super[ArrayExpressions].TypeApi with super[OpenCLBooleanExpressions].TypeApi {
    this: Type =>

  }

  type Type <: (Expression with Any) with TypeApi
  protected trait ValueTypeApi extends TypeApi with super.ValueTypeApi { elementType: ValueType =>

    protected trait ArrayTypeApi[NumberOfDimensions <: Nat]
        extends super.ArrayTypeApi[NumberOfDimensions]
        with TypeApi { arrayType: ArrayType[NumberOfDimensions] =>

      type AffineTransform = Sized[IndexedSeq[Sized[Double, Succ[NumberOfDimensions]]], NumberOfDimensions]

      override def toCode(context: Context): Type.Code = {
        val element = context.get(elementType)
        Type.Code(
          globalDeclarations = Fastring.empty,
          globalDefinitions = fast"typedef global ${element.packed} * $name;",
          Type.Accessor.Atom(name)
        )

      }

      protected trait TypedTermApi extends super[TypeApi].TypedTermApi with super[ArrayTypeApi].TypedTermApi {
        this: TypedTerm =>
      }

      type TypedTerm <: (Term with Any) with TypedTermApi

      protected trait DereferenceApi extends super.DereferenceApi with elementType.TypedTermApi {
        this: elementType.TypedTerm =>
        def toCode(context: Context): Term.Code = ???
      }

      type Dereference <: (elementType.TypedTerm with Any) with DereferenceApi

    }

    type ArrayType[NumberOfDimensions <: Nat] <: (Type with Any) with ArrayTypeApi[NumberOfDimensions]

  }
  type ValueType <: (Type with Any) with ValueTypeApi

}
