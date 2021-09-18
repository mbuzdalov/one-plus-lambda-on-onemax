lazy val commonSettings = Seq(
  organization := "com.github.mbuzdalov",
  libraryDependencies ++= Seq(scalaTest, spire),
  scalaVersion := "2.13.6",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.2.10" % Test
lazy val spire = "org.typelevel" %% "spire" % "0.17.0"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "one-plus-lambda-on-onemax", version := "0.0.0")
