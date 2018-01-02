package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject
import shapeless.{Nat, Sized, Witness}

import scala.language.higherKinds

/**
  * @author 杨博 (Yang Bo)
  */
trait PointerTerms extends BooleanExpressions {

  protected trait PointerTermApi[Element <: DslExpression, NumberOfDimensions <: Nat] {

    @inject
    protected def witnessNumberOfDimensions: Witness.Aux[NumberOfDimensions]

    final def numberOfDimensions: NumberOfDimensions = witnessNumberOfDimensions.value

    def isOutOfBound: DslBoolean.DslExpression

    def dereference: Element

    def +(offset: Sized[Seq[Int], NumberOfDimensions]): PointerTerm[Element, NumberOfDimensions]

  }

  /** @template */
  type PointerTerm[Element <: DslExpression, NumberOfDimensions <: Nat] <: PointerTermApi[
    Element,
    NumberOfDimensions]

  /** @template */
  protected type PointerTermCompanion <: AnyRef

  @inject
  protected def PointerTermCompanion: Factory.Nullary[PointerTermCompanion]

  val PointerTerm: PointerTermCompanion = PointerTermCompanion.newInstance()

}
