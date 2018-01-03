package com.thoughtworks.expressions

import scala.language.higherKinds
import com.thoughtworks.feature.{Factory, ImplicitApply}

// TODO: Move to feature.scala
/**
  * @author 杨博 (Yang Bo)
  */
trait Anonymous extends Any

object Anonymous {

  protected sealed trait ImplicitlyCompanion {

    /** Shadow [[Anonymous.implicitValue]] in case of compiler crash when looking for [[scala.languageFeature.higherKinds]] */
    private def implicitValue(): Unit = {}

    type OpaqueType[+A] <: A with Anonymous

    def apply[A](underlying: A with Anonymous): OpaqueType[A]

  }

  protected val Implicitly: ImplicitlyCompanion = new ImplicitlyCompanion {
    type OpaqueType[+A] = A with Anonymous

    def apply[A](underlying: A with Anonymous): OpaqueType[A] = underlying
  }

  type Implicitly[A] = Implicitly.OpaqueType[A]

  implicit def implicitValue[A, Constructor, ImplicitApplied](
      implicit factory: Factory.Aux[(A with Anonymous), Constructor],
      implicitApply: ImplicitApply.Aux[Constructor, ImplicitApplied],
      asImplicitValue: ImplicitApplied <:< (A with Anonymous)
  ): Implicitly[A] = {
    Implicitly(asImplicitValue(implicitApply(factory.newInstance)))
  }

}
