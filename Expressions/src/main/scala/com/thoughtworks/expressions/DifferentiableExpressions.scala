package com.thoughtworks.expressions

import scala.collection.JavaConverters._
import scala.collection.mutable
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

  type DifferentiableContext = mutable.Map[Term, Term]

  def delta(output: Term, knownDeltas: (Term, Term)*): output.DeltaTerm = {
    val context = new java.util.IdentityHashMap[Term, Term].asScala
    context ++= knownDeltas
    val outputDelta = context.getOrElseUpdate(output, output.computeDelta(context))

    // This cast may be avoided by introducing HMap in the future.
    // However, current version of shapeless.HMap does not support mutable maps.
    outputDelta.asInstanceOf[output.DeltaTerm]
  }

}
