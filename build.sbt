name := "scala-learning-sbt"

version := "1.0"

scalaVersion := "2.10.4"

val algebraVersion = "0.2.0-SNAPSHOT"
val catsVersion    = "0.1.0-SNAPSHOT"

libraryDependencies ++=
  Seq(
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
    , "org.spire-math" %% "algebra" % algebraVersion
    , "org.spire-math" %% "algebra-std" % algebraVersion
    , "org.spire-math" %% "cats-core" % catsVersion
    , "org.spire-math" %% "cats-std" % catsVersion
  )