lazy val commonSettings = Seq(
  organization := "com.github.mbuzdalov",
  libraryDependencies ++= Seq(scalaTest, spire),
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.1.0" % Test
lazy val spire = "org.typelevel" % "spire_2.13" % "0.17.0-M1"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "one-plus-lambda-on-onemax", version := "0.0.0")
