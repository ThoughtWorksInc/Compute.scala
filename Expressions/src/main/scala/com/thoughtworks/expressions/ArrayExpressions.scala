package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, Factory2, inject}
import shapeless.{Lazy, Nat, Sized, Witness}
import shapeless.nat._
import shapeless.ops.nat.ToInt
import shapeless.syntax.inject

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayExpressions extends BooleanExpressions {

  protected trait ValueTypeApi extends super.ValueTypeApi { elementType: ValueType =>

    protected trait ArrayTypeApi[NumberOfDimensions <: Nat] extends TypeApi {
      arrayType: ArrayType[NumberOfDimensions] =>

      val operand0: Sized[IndexedSeq[Int], NumberOfDimensions]

      @inject
      protected def numberOfDimensionsToInt: ToInt[NumberOfDimensions]

      final def numberOfDimensions: Int = numberOfDimensionsToInt()

      protected trait TypedTermApi extends TermApi with ArrayTypeApi.super.TypedTermApi {
        this: TypedTerm =>
        def isOutOfBound: BooleanTerm = ???

        def dereference(implicit debuggingInformation: Implicitly[DebuggingInformation]): elementType.TypedTerm = {
          Dereference(this)
        }
        def +(offset: Sized[IndexedSeq[Int], NumberOfDimensions]): TypedTerm = ???

      }

      type TypedTerm <: (Term with Any) with TypedTermApi

      protected trait DereferenceApi {
        val operand0: TypedTerm

      }
      type Dereference <: elementType.TypedTerm with DereferenceApi

      @inject
      def Dereference: Operator1[TypedTerm, Dereference]
    }

    /** @template */
    type ArrayType[NumberOfDimensions <: Nat] <: (Type with Any) with ArrayTypeApi[NumberOfDimensions]

    @inject
    protected def array1dFactory: Factory2[Implicitly[DebuggingInformation], Sized[IndexedSeq[Int], _1], ArrayType[_1]]

    val array1d = Operator1.operator1(array1dFactory)

    @inject
    protected def array2dFactory: Factory2[Implicitly[DebuggingInformation], Sized[IndexedSeq[Int], _2], ArrayType[_2]]

    val array2d = Operator1.operator1(array2dFactory)

    @inject
    protected def array3dFactory: Factory2[Implicitly[DebuggingInformation], Sized[IndexedSeq[Int], _3], ArrayType[_3]]

    /**
      * @note I hope we can inject array3d directly:
      *       {{{
      *       @inject
      *       val array3d: Operator1[Sized[IndexedSeq[Int], shapeless.nat._3], ArrayType[shapeless.nat._3]]
      *       }}}
      *       However, the above code does not compile on Scala 2.12.4 and 2.11.12 .
      *
      *       As a workaround, we inject an [[array3dFactory]] instead, then manually creates this [[array3d]]
      */
    val array3d = Operator1.operator1(array3dFactory)

    // TODO: I don't like the injected `ToInt`. It's better to create a path-dependent type based Nat instead.
    @inject
    def arrayFactory[NumberOfDimensions <: Nat: ToInt]: Factory2[Implicitly[DebuggingInformation],
                                                                 Sized[IndexedSeq[Int], NumberOfDimensions],
                                                                 ArrayType[NumberOfDimensions]]

    def array[NumberOfDimensions <: Nat: ToInt](dimensions: Sized[IndexedSeq[Int], NumberOfDimensions])(
        implicit debuggingInformation: Implicitly[DebuggingInformation]) = {
      arrayFactory[NumberOfDimensions].newInstance(debuggingInformation, dimensions)
    }

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
