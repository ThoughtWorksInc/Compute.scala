crossScalaVersions := Seq("2.11.11", "2.12.3")

organization in ThisBuild := "com.thoughtworks.compute"

lazy val Memory = project

lazy val Closeables = project

lazy val OpenCL = project.dependsOn(Closeables, Memory)

lazy val OpenCLCodeGenerator = project.dependsOn(Memory)
