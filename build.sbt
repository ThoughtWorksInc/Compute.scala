organization in ThisBuild := "com.thoughtworks.compute"

lazy val Memory = project

lazy val OpenCL = project.dependsOn(Memory)

lazy val Benchmark = project.dependsOn(OpenCL)

lazy val Expressions = project

lazy val Trees = project.dependsOn(Expressions)

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
