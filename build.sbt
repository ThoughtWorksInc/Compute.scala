lazy val Expressions = project

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

organization in ThisBuild := "com.thoughtworks.expression"
