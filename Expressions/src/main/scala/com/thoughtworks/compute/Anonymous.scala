package com.thoughtworks.compute

import scala.language.higherKinds
import com.thoughtworks.feature.{Factory, ImplicitApply}

// TODO: Move to feature.scala
/**
  * @author 杨博 (Yang Bo)
  */
trait Anonymous extends Any

object Anonymous {

  type Implicitly[A] = A with Anonymous

  implicit def implicitValue[A, Constructor, ImplicitApplied](
      implicit factory: Factory.Aux[(A with Anonymous), Constructor],
      implicitApply: ImplicitApply.Aux[Constructor, ImplicitApplied],
      asImplicitValue: ImplicitApplied <:< (A with Anonymous)
  ): Implicitly[A] = {
    asImplicitValue(implicitApply(factory.newInstance))
  }

}
