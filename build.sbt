val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fpis",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.18.0" % Test
  )
