package com.thoughtworks.expressions.api

/**
  * @author 杨博 (Yang Bo)
  */
trait Arrays extends Values {

  protected trait ValueApi extends super.ValueApi { thisValue: ValueTerm =>
    def fill(shape: Int*): ArrayTerm {
      type Element = thisValue.Self
    }
  }

  override type ValueTerm <: (Term with Any) with ValueApi

  protected trait ArrayApi { thisArray =>
    type Element <: ValueTerm

    def shape: Array[Int]

    def extract: Element

  }

  type ArrayTerm <: (Term with Any) with ArrayApi

}
