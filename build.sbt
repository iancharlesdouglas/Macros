name := "Macros"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.12.6",
  "org.scala-lang" % "scala-compiler" % "2.12.6" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test")