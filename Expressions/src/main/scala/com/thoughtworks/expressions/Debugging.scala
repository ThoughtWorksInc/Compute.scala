package com.thoughtworks.expressions
import scala.language.higherKinds

import com.thoughtworks.feature.{Factory, ImplicitApply}

object Debugging {

  trait Line {
    implicit def line: sourcecode.Line
  }

  trait File {
    implicit def file: sourcecode.File
  }

  trait Name {
    implicit def name: sourcecode.Name
  }

  trait FullName {
    implicit def fullName: sourcecode.FullName
  }

//  private[Debugging] trait OpaqueTypes {
//    type Opaque[+DebuggingInformation] <: DebuggingInformation
//
//    def summonOpaque[DebuggingInformation, DebuggingInformationConstructor, ImplicitApplied](
//        implicit debuggingInformationFactory: Factory.Aux[Opaque[DebuggingInformation],
//                                                          DebuggingInformationConstructor],
//        implicitApply: ImplicitApply.Aux[DebuggingInformationConstructor, ImplicitApplied],
//        asDebuggingInformation: ImplicitApplied <:< Opaque[DebuggingInformation]): Opaque[DebuggingInformation] = {
//      implicitApply(debuggingInformationFactory.newInstance)
//    }
//  }
//
//  private[Debugging] val opaqueTypes: OpaqueTypes = new OpaqueTypes {
//    type Opaque[DebuggingInformation] = DebuggingInformation
//  }
//
//  import opaqueTypes._
//  implicit def debugging[DebuggingInformation, DebuggingInformationConstructor, ImplicitApplied](
//      implicit debuggingInformationFactory: Factory.Aux[Opaque[DebuggingInformation], DebuggingInformationConstructor],
//      implicitApply: ImplicitApply.Aux[DebuggingInformationConstructor, ImplicitApplied],
//      asDebuggingInformation: ImplicitApplied <:< Opaque[DebuggingInformation]): Opaque[DebuggingInformation] = {
//    summonOpaque(debuggingInformationFactory, implicitApply, asDebuggingInformation)
//  }

}
