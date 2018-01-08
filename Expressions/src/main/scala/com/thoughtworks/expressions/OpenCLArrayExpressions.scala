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
      def toCode(context: Context): Term.Code = {
//        val name = context.freshName("getElement")
//        val typeReference = context.get(elementType)
//        val packedType = typeReference.packed
//        Code(
//          localDefinitions = fastraw"""
//  $packedType $name = ${context.get(operand0).packed}[${context.get(operand1).packed}];""",
//          accessor = Packed(Fastring(name), typeReference.unpacked.length)
//        )
//        val name = freshName("extr")
        val typeReference = context.get(elementType)
        val packedType = typeReference.packed

        val arrayTerm = operand0

        // TODO: check boundary
        val globalIndices = for {
          i <- 0 until arrayTerm.`type`.shape.length
        } yield fast"[get_global_id($i)]"
        Term.Code(
          localDefinitions = fastraw"""
            $packedType $name = (*${context.get(arrayTerm).packed})${globalIndices.mkFastring};
          """,
          accessor = Term.Accessor.Atom(fast"$name")
        )
      }
    }

    type ExtractFromArrayBuffer <: (TypedTerm with Any) with ExtractFromArrayBufferApi

  }

  type ValueType <: (Type with Any) with ValueTypeApi

  protected trait ArrayBufferTypeApi extends super.ArrayBufferTypeApi with TypeApi { arrayType: ArrayBufferType =>

    override def toCode(context: Context): Type.Code = {
      val element = context.get(operand0)
      Type.Code(
        globalDefinitions =
          fast"typedef global ${element.packed} (* $name)${for (size <- arrayType.shape) yield fast"[$size]"};",
        accessor = Type.Accessor.Atom(name)
      )
    }

    protected trait IdentifierApi extends super.IdentifierApi with TypedTermApi { this: Identifier =>
//        def matrix: TransformationMatrix = TransformationMatrix.identity
    }

    type Identifier <: (TypedTerm with Any) with IdentifierApi

  }

  type ArrayBufferType <: (ArrayType with Any) with ArrayBufferTypeApi

}
