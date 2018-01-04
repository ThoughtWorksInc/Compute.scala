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
trait PointerExpressions extends BooleanExpressions {

  protected trait ValueTypeApi { elementType: ValueType =>

    protected trait PointerTypeApi[NumberOfDimensions <: Nat] extends TypeApi {
      pointerType: PointerType[NumberOfDimensions] =>
      @inject
      protected def witnessNumberOfDimensions: Witness.Aux[NumberOfDimensions]

      final def numberOfDimensions: NumberOfDimensions = witnessNumberOfDimensions.value

      trait TypedTermApi extends TermApi with PointerTypeApi.super.TypedTermApi {
        this: TypedTerm =>
        def isOutOfBound: boolean.TypedTerm = ???

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
    type PointerType[NumberOfDimensions <: Nat] <: (Type with Any) with PointerTypeApi[NumberOfDimensions]
    @inject
    def pointer1dFactory: Factory1[DebuggingInformation, PointerType[_1]]

    val pointer1d: PointerType[_1] = pointer1dFactory.newInstance(debuggingInformation)

    @inject
    def pointer2dFactory: Factory1[DebuggingInformation, PointerType[_2]]

    val pointer2d: PointerType[_2] = pointer2dFactory.newInstance(debuggingInformation)

    @inject
    def pointer3dFactory: Factory1[DebuggingInformation, PointerType[_3]]

    val pointer3d: PointerType[_3] = pointer3dFactory.newInstance(debuggingInformation)

    final def pointer[NumberOfDimensions <: Nat](
        implicit factory: Factory.Lt[PointerType[NumberOfDimensions],
                                     (DebuggingInformation, this.type) => PointerType[NumberOfDimensions]])
      : PointerType[NumberOfDimensions] = {
      factory.newInstance(debuggingInformation, this)
    }

  }

  type ValueType <: (Type with Any) with ValueTypeApi

}
