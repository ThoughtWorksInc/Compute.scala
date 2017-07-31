lazy val Memory = project

lazy val Closeables = project

lazy val OpenCL = project.dependsOn(Closeables, Memory)

lazy val OpenCLCodeGenerator = project.dependsOn(Memory)
