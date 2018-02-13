enablePlugins(JmhPlugin)

libraryDependencies += ("org.lwjgl" % "lwjgl" % "3.1.5").jar().classifier {
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

libraryDependencies += "com.dongxiguo" %% "fastring" % "0.3.1"

sourceDirectory in Jmh := (sourceDirectory in Test).value

classDirectory in Jmh := (classDirectory in Test).value

dependencyClasspath in Jmh := (dependencyClasspath in Test).value
