enablePlugins(JmhPlugin)

libraryDependencies += "org.nd4j" % "nd4j-api" % "0.8.0"

libraryDependencies += "org.nd4j" % "nd4j-cuda-8.0-platform" % "0.8.0"

libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "0.8.0"

libraryDependencies += ("org.lwjgl" % "lwjgl" % "3.1.6").jar().classifier {
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

scalacOptions += "-Ypartial-unification"
