package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject
import shapeless.{Nat, Sized, Witness}

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait PointerExpressions extends BooleanExpressions {

  protected trait PointerExpressionApi[Element <: DslExpression, NumberOfDimensions <: Nat] {

    @inject
    protected def witnessNumberOfDimensions: Witness.Aux[NumberOfDimensions]

    final def numberOfDimensions: NumberOfDimensions = witnessNumberOfDimensions.value

    def isOutOfBound: DslBoolean.DslExpression

    def dereference: Element

    def +(offset: Sized[Seq[Int], NumberOfDimensions]): PointerExpression[Element, NumberOfDimensions]

  }

  /** @template */
  type PointerExpression[Element <: DslExpression, NumberOfDimensions <: Nat] <: PointerExpressionApi[
    Element,
    NumberOfDimensions]

  /** @template */
  protected type PointerExpressionCompanion <: AnyRef

  @inject
  protected def PointerExpressionCompanion: Factory.Nullary[PointerExpressionCompanion]

  val PointerExpression: PointerExpressionCompanion = PointerExpressionCompanion.newInstance()

}
