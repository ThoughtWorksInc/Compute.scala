package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.{Factory, ImplicitApply}
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait Expressions {

  @inject
  val debuggingInformation: Implicitly[DebuggingInformation]

  object Operator0 {
    implicit def operator0[Out, Constructor](
        implicit factory: Factory.Unary[DebuggingInformation, Out]): Operator0[Out] =
      new Operator0[Out] {
        def apply()(implicit debuggingInformation: Implicitly[DebuggingInformation]): Out = {
          factory.newInstance(debuggingInformation)
        }
      }
  }

  trait Operator0[Out] {
    def apply()(implicit debugging: Implicitly[DebuggingInformation]): Out
  }

  /** @template */
  type DebuggingInformation <: AnyRef
  protected trait ExpressionApi {
    val debuggingInformation: DebuggingInformation
  }

  /** @template */
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
