package com.thoughtworks.expressions.opencl

import com.thoughtworks.expressions.api.Terms

/**
  * @author 杨博 (Yang Bo)
  */
trait FunctionContext extends Terms {

  def freshName(prefix: String): String

//  def resolve(id: Any): DslExpression.Accessor
//
//  def get(dslFunction: DslExpression): DslExpression.Accessor
//  def get(dslType: DslType): DslType.Accessor
//  def get(effect: DslEffect): DslEffect.Statement
}
