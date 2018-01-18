package com.thoughtworks.expressions
import com.thoughtworks.feature.Factory.Factory0
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
