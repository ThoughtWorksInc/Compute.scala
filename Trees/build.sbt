libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

scalacOptions += "-Xexperimental"

scalacOptions += "-Ypartial-unification"

addCompilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.3.4")

libraryDependencies += "com.github.ghik" %% "silencer-lib" % "1.3.4"
