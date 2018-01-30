package com.thoughtworks.compute.api

/**
  * @author 杨博 (Yang Bo)
  */
trait FloatArrays extends Floats with Arrays {
  type Category >: this.type <: Floats with Arrays
}
