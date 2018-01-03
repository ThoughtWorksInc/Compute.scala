package com.thoughtworks.expressions

import com.thoughtworks.feature.{Factory, ImplicitApply}
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait Expressions {

  @inject
  protected def debuggingInformationImplicitAppliedFactory: ImplicitAppliedFactory[DebuggingInformation]
  val debuggingInformation: DebuggingInformation = debuggingInformationImplicitAppliedFactory()

  object Operator0 {
    implicit def operator0[Out, Constructor](
        implicit factory: Factory.Aux[Out, Constructor],
        asConstructor: Constructor <:< (DebuggingInformation => Out)): Operator0[Out] =
      new Operator0[Out] {
        def apply()(implicit debugging: ImplicitAppliedFactory[DebuggingInformation]): Out = {
          asConstructor(factory.newInstance)(debugging())
        }
      }
  }

  trait Operator0[Out] {
    def apply()(implicit debugging: ImplicitAppliedFactory[DebuggingInformation]): Out
  }

  /** @template */
  type DebuggingInformation <: AnyRef
  protected trait ExpressionApi {
    val debuggingInformation: DebuggingInformation
  }
  type Expression <: ExpressionApi

  // TODO: Rename DslExpression to Term
  /** @template */
  type DslExpression <: Expression

  /** @template */
  protected type DslExpressionCompanion <: AnyRef

  @inject
  protected def DslExpressionCompanion: Factory.Nullary[DslExpressionCompanion]

  val DslExpression: DslExpressionCompanion = DslExpressionCompanion.newInstance()

  /** @template */
  type Identifier <: DslExpression

  protected trait DslTypeApi extends ExpressionApi {

    type DslExpression <: Expressions.this.DslExpression

    /** @template */
    type Identifier <: DslExpression with Expressions.this.Identifier

    // FIXME: Some identifiers need additional settings,
    // so the arity may be not nullary,
    // and this method will be removed then.
    @inject
    def Identifier: Operator0[Identifier]

  }

  /** @template */
  type DslType <: (Expression with Any) with DslTypeApi // TODO: Rename to Type

  /** @template */
  protected type DslTypeCompanion <: AnyRef // TODO: Rename to TypeCompanion

  @inject
  protected def DslTypeCompanion: Factory.Nullary[DslTypeCompanion]

  val DslType: DslTypeCompanion = DslTypeCompanion.newInstance()

}
