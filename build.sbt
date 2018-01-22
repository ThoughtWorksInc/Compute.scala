crossScalaVersions := Seq("2.12.4", "2.11.11")

organization in ThisBuild := "com.thoughtworks.compute"

lazy val Memory = project

lazy val OpenCL = project.dependsOn(Memory)

lazy val OpenCLCodeGenerator = project.dependsOn(Memory)

lazy val Benchmark = project.dependsOn(OpenCL)
