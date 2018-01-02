package com.thoughtworks.expressions

import com.thoughtworks.feature.Factory.inject

object Debugging {

  trait Line {
    @inject
    def line: sourcecode.Line
  }

  trait File {
    @inject
    def file: sourcecode.File
  }

  trait Name {
    @inject
    def name: sourcecode.Name
  }

  trait FullName {
    @inject
    def fullName: sourcecode.FullName
  }

}

///**
//  * @author 杨博 (Yang Bo)
//  */
//trait Debugging extends Expressions {
//  type DebuggingInformation
//  trait DslExpressionApi  {
//    def debuggingInformation: DebuggingInformation
//  }
//  type DslExpression <: DslExpressionApi
//}
