package com.thoughtworks.expressions

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
