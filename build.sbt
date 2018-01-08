lazy val Expressions = project

SettingKey[Seq[String]]("cross-typelevel-scala-versions") in ThisBuild := Seq("2.11.11-bin-typelevel-4")

crossScalaVersions in ThisBuild := {
  Seq("2.11.12", "2.12.4") ++ (SettingKey[Seq[String]]("cross-typelevel-scala-versions") in ThisBuild).value
}
