package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory1, Factory2, inject}
import shapeless.{Lazy, Nat, Sized, Witness}
import shapeless.nat._

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait ArrayExpressions extends BooleanExpressions {

  protected trait ValueTypeApi extends super.ValueTypeApi { elementType: ValueType =>

    protected trait ArrayTypeApi[NumberOfDimensions <: Nat] extends TypeApi {
      arrayType: ArrayType[NumberOfDimensions] =>
      @inject
      protected def witnessNumberOfDimensions: Witness.Aux[NumberOfDimensions]

      final def numberOfDimensions: NumberOfDimensions = witnessNumberOfDimensions.value

      trait TypedTermApi extends TermApi with ArrayTypeApi.super.TypedTermApi {
        this: TypedTerm =>
        def isOutOfBound: BooleanTerm = ???

        def dereference(implicit debuggingInformation: Implicitly[DebuggingInformation]): elementType.TypedTerm = {
          Dereference(this)
        }
        def +(offset: Sized[Seq[Int], NumberOfDimensions]): TypedTerm = ???

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
    def array1dFactory: Factory1[DebuggingInformation, ArrayType[_1]]

    val array1d: ArrayType[_1] = array1dFactory.newInstance(debuggingInformation)

    @inject
    def array2dFactory: Factory1[DebuggingInformation, ArrayType[_2]]

    val array2d: ArrayType[_2] = array2dFactory.newInstance(debuggingInformation)

    @inject
    def array3dFactory: Factory1[DebuggingInformation, ArrayType[_3]]

    val array3d: ArrayType[_3] = array3dFactory.newInstance(debuggingInformation)

    final def array[NumberOfDimensions <: Nat](
        implicit factory: Factory.Lt[ArrayType[NumberOfDimensions],
                                     (DebuggingInformation, this.type) => ArrayType[NumberOfDimensions]])
      : ArrayType[NumberOfDimensions] = {
      factory.newInstance(debuggingInformation, this)
    }

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
