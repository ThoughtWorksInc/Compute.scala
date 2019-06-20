scalacOptions += "-Ypartial-unification"

libraryDependencies += "com.google.guava" % "guava" % "28.0-jre"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

libraryDependencies += ("org.lwjgl" % "lwjgl" % "3.2.2" % Optional).jar().classifier {
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

addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.3.4")

libraryDependencies += "com.github.ghik" %% "silencer-lib" % "1.3.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % Test

fork in Test := true

enablePlugins(Example)
