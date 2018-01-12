package com.thoughtworks.expressions

object Builtins {

  /**
    * @author 杨博 (Yang Bo)
    */
  trait AllOpenCLValues extends OpenCLArrays with OpenCLFloats with FreshNames

  trait AllDifferentiableExpressions
      extends DifferentiableArrays
      with DifferentiableFloats
      with DifferentiableBooleans

}
