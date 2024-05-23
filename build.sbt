val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fpis",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Wunused:imports",
  "-Wunused:privates",
  "-Wunused:params",
  "-Wunused:locals"
)
