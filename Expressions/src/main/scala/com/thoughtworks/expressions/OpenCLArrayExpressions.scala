package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import shapeless.ops.nat.ToInt
import shapeless.{Nat, Sized, Succ}

import scala.collection.{AbstractSeq, IndexedSeqOptimized}
import scala.language.higherKinds
//object OpenCLArrayExpressions {
//
//
//
//  /** A n-dimensional affine transform that performs a linear mapping from n-dimensional coordinates to other n-dimensional coordinates
//    *
//    * This [[TransformationMatrix]] is similar to [[java.awt.geom.AffineTransform]],
//    * except this [[TransformationMatrix]] is generic for any n-dimensional coordinates, not only 2D coordinates.
//    */
//  type TransformationMatrix[NumberOfDimensions <: Nat] =
//    Sized[IndexedSeq[Sized[IndexedSeq[Double], Succ]], NumberOfDimensions]
//
//  object TransformationMatrix {
//    def identity[NumberOfDimensions <: Nat: ToInt]: TransformationMatrix = {
//      val numberOfDimensions = ToInt.apply()
//      type Row = Sized[IndexedSeq[Double], Succ]
//      val matrixData: IndexedSeq[Row] = new AbstractSeq[Row] with IndexedSeq[Row] {
//        def length: Int = numberOfDimensions
//        def apply(y: Int): Row = {
//          val rowData: IndexedSeq[Double] = new AbstractSeq[Double] with IndexedSeq[Double] {
//            def length: Int = numberOfDimensions + 1
//            def apply(x: Int): Double = if (x == y) 1.0 else 0.0
//          }
//          Sized.wrap(rowData)
//        }
//      }
//      Sized.wrap(matrixData)
//    }
//  }
//
//}

/**
  * @author 杨博 (Yang Bo)
  */
trait OpenCLArrayExpressions extends ArrayExpressions with OpenCLBooleanExpressions {

  protected trait TypeApi extends super[ArrayExpressions].TypeApi with super[OpenCLBooleanExpressions].TypeApi {
    this: Type =>

  }

  type Type <: (Expression with Any) with TypeApi
  protected trait ValueTypeApi extends TypeApi with super.ValueTypeApi { elementType: ValueType =>

    protected trait ArrayTypeApi
        extends super.ArrayTypeApi
        with TypeApi { arrayType: ArrayType =>

      override def toCode(context: Context): Type.Code = {
        val element = context.get(elementType)
        Type.Code(
          globalDeclarations = Fastring.empty,
          globalDefinitions =
            fast"typedef global ${element.packed} (* $name)${for (size <- arrayType.shape) yield fast"[$size]"};",
          Type.Accessor.Atom(name)
        )
      }

      protected trait TypedTermApi extends ArrayTermApi with super[TypeApi].TypedTermApi with super[ArrayTypeApi].TypedTermApi {
        this: TypedTerm =>
//        def matrix: TransformationMatrix
      }

      type TypedTerm <: (ArrayTerm with Any) with TypedTermApi

      protected trait ExtractApi extends super.ExtractApi with elementType.TypedTermApi {
        this: elementType.TypedTerm =>
        def toCode(context: Context): Term.Code = ???
      }

      type Extract <: (elementType.TypedTerm with Any) with ExtractApi

      protected trait IdentifierApi extends super.IdentifierApi with TypedTermApi { this: Identifier =>
//        def matrix: TransformationMatrix = TransformationMatrix.identity
      }

      type Identifier <: (TypedTerm with Any) with IdentifierApi

    }

    type ArrayType <: (Type with Any) with ArrayTypeApi

  }
  type ValueType <: (Type with Any) with ValueTypeApi

}
