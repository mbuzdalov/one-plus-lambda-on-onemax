lazy val commonSettings = Seq(
  organization := "com.github.mbuzdalov",
  libraryDependencies ++= Seq(apacheMath, scalaTest, spire),
  scalaVersion := "2.13.4",
  scalacOptions ++= Seq("-deprecation"),
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.2.3" % Test
lazy val apacheMath = "org.apache.commons" % "commons-math3" % "3.6.1"
lazy val spire = "org.typelevel" %% "spire" % "0.17.0"

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "one-plus-lambda-on-onemax", version := "0.0.0")
