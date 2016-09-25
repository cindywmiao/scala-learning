name := "scala-learning-sbt"

version := "1.0"

scalaVersion := "2.10.4"

version := "0.2.5-SNAPSHOT"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0",
  "org.scala-exercises" %% "exercise-compiler" % version.value,
  "org.scala-exercises" %% "definitions" % version.value
  )
