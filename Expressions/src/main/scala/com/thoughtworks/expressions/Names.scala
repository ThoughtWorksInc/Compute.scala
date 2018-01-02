package com.thoughtworks.expressions

/**
  * @author 杨博 (Yang Bo)
  */
trait Names extends Expressions {

  type DebuggingInformation <: Debugging.Name
  trait ExpressionApi extends super.ExpressionApi {
    def name: String = debuggingInformation.name.value
  }
  type Expression <: ExpressionApi

}
