organization in ThisBuild := "com.thoughtworks.compute"

lazy val Memory = project

lazy val OpenCL = project.dependsOn(Memory)

lazy val benchmarks = project.dependsOn(Tensors)

lazy val Expressions = project.dependsOn(NDimensionalAffineTransform)

lazy val Trees = project.dependsOn(Expressions)

lazy val NDimensionalAffineTransform = project

lazy val OpenCLKernelBuilder = project.dependsOn(Expressions, Trees % Test)

lazy val Tensors = project.dependsOn(OpenCLKernelBuilder, OpenCL, Trees)

lazy val gpu = project.dependsOn(Tensors)

lazy val cpu = project.dependsOn(Tensors)

publish / skip := true

enablePlugins(ScalaUnidocPlugin)

unidocProjectFilter in ScalaUnidoc in unidoc := inAggregates(LocalRootProject)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

scalacOptions += "-Xexperimental"

scalacOptions += "-Ypartial-unification"

scalacOptions in ThisBuild ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Some("-Ybackend:GenBCode")
  } else {
    None
  }
}
