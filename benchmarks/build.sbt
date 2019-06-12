enablePlugins(JmhPlugin)

libraryDependencies += "org.nd4j" % "nd4j-api" % "0.8.0"

val nd4jRuntime = settingKey[String]("\"cuda-8.0\" to run benchmark on GPU, \"native\" to run benchmark on CPU.")

nd4jRuntime in Global := "native"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "org.nd4j" % s"nd4j-${nd4jRuntime.value}-platform" % "0.8.0"

libraryDependencies += ("org.lwjgl" % "lwjgl" % "3.2.1").jar().classifier {
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

addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.3.4")
