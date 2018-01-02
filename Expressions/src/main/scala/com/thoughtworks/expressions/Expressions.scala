package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

//trait Low {
//  type Factory.Nullary[Output] = Factory.Lt[Output, () => Output]
//  type Factory.Lt[Output, +Constructor0] = Factory[Output] {
//    type Constructor <: Constructor0
//  }
//
//  def factoryLt[Output, Constructor0, Constructor1](
//      implicit factory: Factory.Aux[Output, Constructor0],
//      isLt: Factory.Aux[Output, Constructor0] <:< Factory.Lt[Output, Constructor1]): Factory.Lt[Output, Constructor1] = {
//    isLt(factory)
//  }
//
//}
//object Expressions extends Low {
//
//  trait DebuggingInformationApi
//}

/**
  * @author 杨博 (Yang Bo)
  */
trait Expressions {

  protected object Operator0 {
    implicit def operator0[Out, Constructor](
        implicit factory: Factory.Aux[Out, Constructor],
        asConstructor: Constructor <:< (DebuggingInformation => Out)): Operator0[Out] =
      new Operator0[Out] {
        def apply()(
            implicit debuggingInformationFactory: Factory.Lt[DebuggingInformation, () => DebuggingInformation]): Out = {
          asConstructor(factory.newInstance)(debuggingInformationFactory.newInstance())
        }
      }
  }

  protected trait Operator0[Out] {
    def apply()(implicit debuggingInformationFactory: Factory.Lt[DebuggingInformation, () => DebuggingInformation]): Out
  }

  /** @template */
  type DebuggingInformation <: AnyRef

  protected trait DslExpressionApi {
    val debuggingInformation: DebuggingInformation
  }

  /** @template */
  type DslExpression <: DslExpressionApi
  // TODO: Rename to Expression

  /** @template */
  protected type DslExpressionCompanion <: AnyRef

  @inject
  protected def DslExpressionCompanion: Factory.Nullary[DslExpressionCompanion]

  val DslExpression: DslExpressionCompanion = DslExpressionCompanion.newInstance()

  /** @template */
  type Identifier <: DslExpression

  protected trait DslTypeApi {

    type DslExpression <: Expressions.this.DslExpression

    /** @template */
    type Identifier <: DslExpression with Expressions.this.Identifier

    @inject
    def Identifier: Operator0[Identifier]

  }

  /** @template */
  type DslType <: DslTypeApi // TODO: Rename to Type

  /** @template */
  protected type DslTypeCompanion <: AnyRef // TODO: Rename to TypeCompanion

  @inject
  protected def DslTypeCompanion: Factory.Nullary[DslTypeCompanion]

  val DslType: DslTypeCompanion = DslTypeCompanion.newInstance()

}
