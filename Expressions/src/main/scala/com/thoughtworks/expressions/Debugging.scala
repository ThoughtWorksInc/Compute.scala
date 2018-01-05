package com.thoughtworks.expressions
import com.thoughtworks.expressions.Anonymous.Implicitly
import com.thoughtworks.feature.Factory.inject

import scala.language.higherKinds

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

}

trait Debugging {
  @inject
  val debuggingInformation: Implicitly[DebuggingInformation]

  type DebuggingInformation <: Debugging.Name
  protected trait ExpressionApi {
    val debuggingInformation: DebuggingInformation
    def name: String = debuggingInformation.name.value
  }
  type Expression <: ExpressionApi
}
