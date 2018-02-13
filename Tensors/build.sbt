scalacOptions += "-Ypartial-unification"

libraryDependencies += "com.google.guava" % "guava" % "23.6-jre"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

val lwjglNatives: String = {
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

libraryDependencies += ("org.lwjgl" % "lwjgl" % "3.1.5" % Test).classifier(lwjglNatives).jar()

addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "0.6")

libraryDependencies += "com.github.ghik" %% "silencer-lib" % "0.6"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % Test

fork in Test := true
