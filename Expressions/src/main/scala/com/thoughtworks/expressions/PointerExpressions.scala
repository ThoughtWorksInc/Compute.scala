package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory2, inject}
import shapeless.{Lazy, Nat, Sized, Witness}
import shapeless.nat._

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait PointerExpressions extends BooleanExpressions {

  protected trait PointerTermApi[+ElementType <: ValueType, NumberOfDimensions <: Nat] extends TermApi {
    this: PointerTerm[ElementType, NumberOfDimensions] =>

    val dslType: PointerType[ElementType, NumberOfDimensions]

    def isOutOfBound: boolean.TypedTerm = ???

    def dereference(implicit debuggingInformation: Implicitly[DebuggingInformation])
      : dslType.elementType.Dereference[NumberOfDimensions]

    def +(offset: Sized[Seq[Int], NumberOfDimensions]): Term = ???

  }
  type PointerTerm[+ElementType <: ValueType, NumberOfDimensions <: Nat] <: (Term with Any) with PointerTermApi[
    ElementType,
    NumberOfDimensions]

  protected trait PointerTypeApi[+ElementType <: ValueType, NumberOfDimensions <: Nat] extends DslTypeApi {
    this: PointerType[ElementType, NumberOfDimensions] =>

    val elementType: ElementType

    @inject
    protected def witnessNumberOfDimensions: Witness.Aux[NumberOfDimensions]

    final def numberOfDimensions: NumberOfDimensions = witnessNumberOfDimensions.value

    trait TypedTermApi extends TermApi { this: TypedTerm =>

      def dereference(implicit debuggingInformation: Implicitly[DebuggingInformation])
        : elementType.Dereference[NumberOfDimensions] = {
        val operator1: Operator1[PointerTerm[elementType.type, NumberOfDimensions],
                                 elementType.Dereference[NumberOfDimensions]] = {
          elementType.Dereference[NumberOfDimensions]
        }

        operator1(this)
      }
    }
    type TypedTerm <: (PointerTerm[elementType.type, NumberOfDimensions] with Any) with TypedTermApi

  }

  /** @template */
  type PointerType[+ElementType <: ValueType, NumberOfDimensions <: Nat] <: (DslType with Any) with PointerTypeApi[
    ElementType,
    NumberOfDimensions]

  protected trait ValueTypeApi { element: ValueType =>

    type Dereference[NumberOfDimensions <: Nat] <: TypedTerm

    @inject
    def Dereference[NumberOfDimensions <: Nat]
      : Operator1[PointerTerm[this.type, NumberOfDimensions], Dereference[NumberOfDimensions]]

    @inject
    def pointer1dFactory: Factory2[DebuggingInformation, this.type, PointerType[this.type, _1]]

    val pointer1d: PointerType[this.type, _1] = pointer1dFactory.newInstance(debuggingInformation, this)

    @inject
    def pointer2dFactory: Factory2[DebuggingInformation, this.type, PointerType[this.type, _2]]

    val pointer2d: PointerType[this.type, _2] = pointer2dFactory.newInstance(debuggingInformation, this)

    @inject
    def pointer3dFactory: Factory2[DebuggingInformation, this.type, PointerType[this.type, _3]]

    val pointer3d: PointerType[this.type, _3] = pointer3dFactory.newInstance(debuggingInformation, this)

    final def pointer[NumberOfDimensions <: Nat](
        implicit factory: Factory.Lt[PointerType[this.type, NumberOfDimensions],
                                     (DebuggingInformation, this.type) => PointerType[this.type, NumberOfDimensions]])
      : PointerType[this.type, NumberOfDimensions] = {
      factory.newInstance(debuggingInformation, this)
    }

  }

  type ValueType <: (DslType with Any) with ValueTypeApi

}
