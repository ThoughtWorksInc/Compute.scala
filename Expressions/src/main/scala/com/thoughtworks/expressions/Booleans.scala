package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory.Factory0
import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.{Factory0, Factory1, inject}

/**
  * @author 杨博 (Yang Bo)
  */
trait Booleans extends Values {

  protected trait BooleanTypeApi extends ValueTypeApi { this: BooleanType =>
    type JvmType = Boolean

    def name = "Boolean"

    def zero(implicit debuggingInformationFacotry: ImplicitlyAppliedFactory[DebuggingInformation]): Literal = Literal(false)
  }

  /** @template */
  type BooleanType <: (ValueType with Any) with BooleanTypeApi

  @inject
  protected def BooleanType: Factory0[BooleanType]

  type BooleanTerm = boolean.TypedTerm

  val boolean: BooleanType = BooleanType.newInstance()
}
