ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.4"

lazy val root = (project in file("."))
  .settings(
    name := "pycal",
    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.13.2",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
