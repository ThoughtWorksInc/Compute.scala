package com.thoughtworks.expressions

import com.thoughtworks.feature.{Factory, ImplicitApply}

/**
  * @author 杨博 (Yang Bo)
  */
trait ImplicitlyAppliedFactory[Output] {
  val newInstance: () => Output
}

object ImplicitlyAppliedFactory {

  implicit def implicitlyAppliedFactory[Output, Constructor, ImplicitApplied](
      implicit factory: Factory.Aux[Output, Constructor],
      implicitApply: ImplicitApply.Aux[Constructor, ImplicitApplied],
      asImplicitValue: ImplicitApplied <:< Output
  ): ImplicitlyAppliedFactory[Output] = make {
    asImplicitValue(implicitApply(factory.newInstance))
  }

  def apply[Output](implicit factory: ImplicitlyAppliedFactory[Output]): ImplicitlyAppliedFactory[Output] = factory

  def make[Output](output: => Output) = new ImplicitlyAppliedFactory[Output] {
    val newInstance: () => Output = output _
  }
}
