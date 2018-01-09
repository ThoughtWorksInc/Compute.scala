package com.thoughtworks.expressions

import com.dongxiguo.fastring.Fastring
import com.dongxiguo.fastring.Fastring.Implicits._
import shapeless.ops.nat.ToInt
import shapeless.{Nat, Sized, Succ}

import scala.collection.{AbstractSeq, IndexedSeqOptimized}
import scala.language.higherKinds
import OpenCLExpressions._
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
trait OpenCLArrayExpressions extends OpenCLBooleanExpressions with ArrayExpressions {

  protected trait TypeApi extends super[ArrayExpressions].TypeApi with super[OpenCLBooleanExpressions].TypeApi {
    this: Type =>

  }

  type Type <: (Expression with Any) with TypeApi

  protected trait ValueTypeApi
      extends super[ArrayExpressions].ValueTypeApi
      with super[OpenCLBooleanExpressions].ValueTypeApi { elementType: ValueType =>

    protected trait ExtractFromArrayBufferApi extends super.ExtractFromArrayBufferApi {
      def name: String
      def toCode(context: OpenCLContext): OpenCLTerm.Code = {
        val typeReference = context.get(elementType)
        val packedType = typeReference.packed

        val arrayTerm = operand0

        // TODO: check boundary
        val globalIndices = for {
          i <- arrayTerm.`type`.shape.indices
        } yield fast"[get_global_id($i)]"
        OpenCLTerm.Code(
          localDefinitions = fastraw"""
            $packedType $name = (*${context.get(arrayTerm).packed})${globalIndices.mkFastring};
          """,
          accessor = OpenCLTerm.Accessor.Packed(fast"$name", context.get(arrayTerm.`type`).unpacked.length)
        )
      }
    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait ArrayBufferTypeApi extends super.ArrayBufferTypeApi with TypeApi { arrayType: ArrayBufferType =>

    def toCode(context: OpenCLContext): OpenCLType.Code = {
      val element = context.get(operand0)
      val dimensions = for (size <- arrayType.shape) yield fast"[$size]"
      OpenCLType.Code(
        globalDefinitions = fast"typedef global ${element.packed} (*$name)${dimensions.mkFastring};",
        accessor = OpenCLType.Accessor.Atom(name)
      )
    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

  }

  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

}
