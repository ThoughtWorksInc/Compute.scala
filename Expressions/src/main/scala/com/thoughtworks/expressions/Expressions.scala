package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.{Factory, ImplicitApply}
import com.thoughtworks.feature.Factory.{Factory1, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait Expressions {

  @inject
  val debuggingInformation: Implicitly[DebuggingInformation]

  object Operator0 {
    implicit def operator0[Out](implicit factory: Factory1[DebuggingInformation, Out]): Operator0[Out] =
      new Operator0[Out] {
        def apply()(implicit debuggingInformation: Implicitly[DebuggingInformation]): Out = {
          factory.newInstance(debuggingInformation)
        }
      }
  }

  trait Operator0[Out] {
    def apply()(implicit debugging: Implicitly[DebuggingInformation]): Out
  }

  object Operator1 {
    implicit def operator1[Operand0, Out](
        implicit factory: Factory.Factory2[DebuggingInformation, Operand0, Out]): Operator1[Operand0, Out] =
      new Operator1[Operand0, Out] {
        def apply(operand0: Operand0)(implicit debuggingInformation: Implicitly[DebuggingInformation]): Out = {
          factory.newInstance(debuggingInformation, operand0)
        }
      }
  }

  trait Operator1[Operand0, Out] {
    def apply(operand0: Operand0)(implicit debugging: Implicitly[DebuggingInformation]): Out
  }

  /** @template */
  type DebuggingInformation <: AnyRef
  protected trait ExpressionApi {
    val debuggingInformation: DebuggingInformation
  }

  /** @template */
  type Expression <: ExpressionApi

  protected trait TermApi {
    val dslType: DslType
  }

  // TODO: Rename Term to Term
  /** @template */
  type Term <: (Expression with Any) with TermApi

  /** @template */
  protected type TermCompanion <: AnyRef

  @inject
  protected def TermCompanion: Factory.Factory0[TermCompanion]

  val Term: TermCompanion = TermCompanion.newInstance()

  /** @template */
  type Identifier <: Term

  protected trait DslTypeApi extends ExpressionApi { this: DslType =>

    trait TermApi extends Expressions.this.TermApi {
      val dslType: DslTypeApi.this.type = DslTypeApi.this
    }

    type Term <: (Expressions.this.Term with Any) with TermApi

    /** @template */
    type Identifier <: Term with Expressions.this.Identifier

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
  protected def DslTypeCompanion: Factory.Factory0[DslTypeCompanion]

  val DslType: DslTypeCompanion = DslTypeCompanion.newInstance()

}
