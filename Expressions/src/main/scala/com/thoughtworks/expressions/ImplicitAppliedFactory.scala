package com.thoughtworks.expressions

import com.thoughtworks.feature.{Factory, ImplicitApply}

object ImplicitAppliedFactory {
  implicit def summon[DebuggingInformation, DebuggingInformationConstructor, ImplicitApplied](
      implicit debuggingInformationFactory: Factory.Aux[DebuggingInformation, DebuggingInformationConstructor],
      implicitApply: ImplicitApply.Aux[DebuggingInformationConstructor, ImplicitApplied],
      asDebuggingInformation: ImplicitApplied <:< DebuggingInformation): ImplicitAppliedFactory[DebuggingInformation] =
    new ImplicitAppliedFactory[DebuggingInformation] {
      def apply(): DebuggingInformation = {
        asDebuggingInformation(implicitApply(debuggingInformationFactory.newInstance))
      }
    }

}

trait ImplicitAppliedFactory[A] extends (() => A)
