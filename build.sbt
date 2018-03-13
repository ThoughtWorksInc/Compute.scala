organization in ThisBuild := "com.thoughtworks.compute"

lazy val Memory = project

lazy val OpenCL = project.dependsOn(Memory)

lazy val Benchmark = project.dependsOn(OpenCL)

lazy val benchmarks = project.dependsOn(Tensors)

lazy val Expressions = project.dependsOn(NDimensionalAffineTransform)

lazy val Trees = project.dependsOn(Expressions)

lazy val NDimensionalAffineTransform = project

lazy val OpenCLKernelBuilder = project.dependsOn(Expressions, Trees % Test)

lazy val Tensors = project.dependsOn(OpenCLKernelBuilder, OpenCL, Trees)

val defaultCrossTypeLevelScalaVersions = Seq(
  // "2.12.4-bin-typelevel-4" // Disabled due to https://github.com/typelevel/scala/issues/176
)
val crossLightbendScalaVersions = Seq("2.11.12", "2.12.4")

crossScalaVersions in ThisBuild := {
  crossLightbendScalaVersions ++
    (SettingKey[Seq[String]]("cross-typelevel-scala-versions") in ThisBuild)
      .??(defaultCrossTypeLevelScalaVersions)
      .value
}

publishArtifact := false

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
  .settings(
    unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := inAggregates(LocalRootProject),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    scalacOptions += "-Xexperimental",
    scalacOptions += "-Ypartial-unification"
  )
