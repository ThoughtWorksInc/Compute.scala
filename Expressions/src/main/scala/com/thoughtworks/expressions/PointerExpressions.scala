package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject
import shapeless.{Lazy, Nat, Sized, Witness}
import shapeless.nat._

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait PointerExpressions extends BooleanExpressions {

  protected trait PointerTypeApi[+ElementType <: DslType, NumberOfDimensions <: Nat] extends DslTypeApi {
    this: PointerType[ElementType, NumberOfDimensions] =>

    //    def elementType: element.type = element

    //      @inject
    //      protected def witnessNumberOfDimensions: Witness.Aux[ElementType, NumberOfDimensions]
    //
    //      final def numberOfDimensions: NumberOfDimensions = witnessNumberOfDimensions.value
    //
    protected trait DslExpressionApi {

      //        def isOutOfBound: boolean.DslExpression = ???
      //
      //        def dereference: DslTypeApi.this.DslExpression = ???
      //
      //        def +(offset: Sized[Seq[Int], NumberOfDimensions]): DslExpression = ???

    }

    type DslExpression <: (PointerExpressions.this.DslExpression with Any) with DslExpressionApi

  }

  // FIXME: Fix the bug "addChild inapplicable for type Output" due to NumberOfDimensions
  type PointerType[+ElementType <: DslType, NumberOfDimensions <: Nat] <: (DslType with Any) with PointerTypeApi[
    ElementType,
    NumberOfDimensions]

//
//  /** @template */
//  protected type PointerType[+ElementType <: DslType, NumberOfDimensions <: Nat] <: DslType with PointerTypeApi[
//    ElementType,
//    NumberOfDimensions]

//  type FloatType <: DslType

  protected trait ValueTypeApi { element: ValueType =>

    @inject
    def pointer1dFactory: Factory.Unary[DebuggingInformation, PointerType[this.type, _1]]

    val pointer1d: PointerType[this.type, _1] = pointer1dFactory.newInstance(debuggingInformation)

    @inject
    def pointer2dFactory: Factory.Unary[DebuggingInformation, PointerType[this.type, _2]]

    val pointer2d: PointerType[this.type, _2] = pointer2dFactory.newInstance(debuggingInformation)

    @inject
    def pointer3dFactory: Factory.Unary[DebuggingInformation, PointerType[this.type, _3]]

    val pointer3d: PointerType[this.type, _3] = pointer3dFactory.newInstance(debuggingInformation)

  }

  type ValueType <: (DslType with Any) with ValueTypeApi
  //  protected trait PointerSingletonApi {
//
//    def apply[ElementType, NumberOfDimensions <: Nat](elementType: DslType)(
//      implicit factory: Factory.Unary[DebuggingInformation, PointerType[elementType.type, NumberOfDimensions]])
//    : PointerType[elementType.type, NumberOfDimensions] = factory.newInstance(debuggingInformation)
//
//  }
//  /** @template */
//  protected type PointerSingleton <: PointerSingletonApi
//
//  @inject
//  protected def PointerSingleton: Factory.Nullary[PointerSingleton]
//
//  val pointer: PointerSingleton = PointerSingleton.newInstance()

}
