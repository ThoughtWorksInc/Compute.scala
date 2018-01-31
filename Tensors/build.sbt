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

fork in Test := true
