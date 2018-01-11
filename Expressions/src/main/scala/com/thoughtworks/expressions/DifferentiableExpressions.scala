package com.thoughtworks.expressions

import scala.collection.JavaConverters._
import scala.language.existentials

/**
  * @author 杨博 (Yang Bo)
  */
trait DifferentiableExpressions extends Expressions {

  protected trait TermApi extends super.TermApi {
    type DeltaTerm = `type`.deltaType.TypedTerm

    // FIXME: `x` should be narrow to FloatTerm or change to Context
    /** Returns the symbolic difference `∂this/∂x` */
    def computeDelta(context: DifferentiableContext): DeltaTerm
  }

  type Term <: (Expression with Any) with TermApi

  protected trait TypeApi extends super.TypeApi { this: Type =>

    val deltaType: Type

  }

  type Type <: (Expression with Any) with TypeApi

  final class DifferentiableContext private (deltas: java.util.IdentityHashMap[Term, Term]) {

    def this(knownDeltas: (Term, Term)*) = this {
      val deltas = new java.util.IdentityHashMap[Term, Term]
      deltas.asScala ++= knownDeltas
      deltas
    }

    def delta(term: Term): term.DeltaTerm = {
      deltas.asScala.getOrElseUpdate(term, term.computeDelta(this)).asInstanceOf[term.DeltaTerm]
    }

  }

  def delta(root: Term, knownDeltas: (Term, Term)*): root.DeltaTerm = {
    new DifferentiableContext(knownDeltas: _*).delta(root)
  }

}
