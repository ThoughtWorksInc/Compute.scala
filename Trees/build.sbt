libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % Test

scalacOptions += "-Xexperimental"

scalacOptions += "-Ypartial-unification"

addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "0.6")

libraryDependencies += "com.github.ghik" %% "silencer-lib" % "0.6"
