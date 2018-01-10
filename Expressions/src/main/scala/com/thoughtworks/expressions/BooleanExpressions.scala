package com.thoughtworks.expressions

import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory0, Factory1, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait BooleanExpressions extends ValueExpressions {

  protected trait BooleanTypeApi extends ValueTypeApi { this: BooleanType =>
    type JvmType = Boolean

    def name = "Boolean"

    def zero(implicit debuggingInformation: Implicitly[DebuggingInformation]): Literal = Literal(false)
  }

  /** @template */
  type BooleanType <: (ValueType with Any) with BooleanTypeApi

  @inject
  protected def BooleanType: Factory0[BooleanType]

  type BooleanTerm = boolean.TypedTerm

  val boolean: BooleanType = BooleanType.newInstance()
}
