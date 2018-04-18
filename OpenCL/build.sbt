libraryDependencies += ("org.lwjgl" % "lwjgl" % "3.1.6" % Test).jar().classifier {
  import scala.util.Properties._
  if (isMac) {
    "natives-macos"
  } else if (isLinux) {
    "natives-linux"
  } else if (isWin) {
    "natives-windows"
  } else {
    throw new MessageOnlyException(s"lwjgl does not support $osName")
  }
}

libraryDependencies += "org.lwjgl" % "lwjgl-opencl" % "3.1.6"

libraryDependencies += "com.thoughtworks.raii" %% "asynchronous" % "3.0.0-M11"

libraryDependencies += "com.thoughtworks.raii" %% "asynchronouspool" % "3.0.0-M11"

libraryDependencies += "com.thoughtworks.feature" %% "partialapply" % "2.3.0-M8"

libraryDependencies += "com.thoughtworks.feature" %% "implicitapply" % "2.3.0-M8"

libraryDependencies += "com.thoughtworks.feature" %% "mixins-implicitssingleton" % "2.3.0-M8"

libraryDependencies += "com.thoughtworks.feature" %% "factory" % "2.3.0-M8"

libraryDependencies += "com.chuusai" %%% "shapeless" % "2.3.3"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

fork := true

scalacOptions += "-Ypartial-unification"

libraryDependencies += "com.thoughtworks.each" %% "each" % "3.3.1"

enablePlugins(Example)
