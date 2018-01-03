package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory
import com.thoughtworks.feature.Factory.inject

/**
  * @author 杨博 (Yang Bo)
  */
trait ValueExpressions extends Expressions {

  /** @template */
  type ValueType <: DslType

//  /** @template */
//  protected type ValueTypeCompanion <: AnyRef // TODO: Rename to TypeCompanion
//
//  @inject
//  protected def ValueTypeCompanion: Factory.Nullary[ValueTypeCompanion]
//
//  val ValueType: ValueTypeCompanion = ValueTypeCompanion.newInstance()

}
