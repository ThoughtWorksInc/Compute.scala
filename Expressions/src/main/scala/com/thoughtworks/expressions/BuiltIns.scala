package com.thoughtworks.expressions

object Builtins {

  /**
    * @author 杨博 (Yang Bo)
    */
  trait AllOpenCLExpressions extends OpenCLArrayExpressions with OpenCLFloatExpressions with FreshNames

  trait AllDifferentiableExpressions extends DifferentiableArrayExpressions

}
