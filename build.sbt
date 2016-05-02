name := """funcProgInScala"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies ++=
  "org.scalatest" %% "scalatest" % "2.2.4" % "test" ::
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test" ::
  Nil

testOptions in Test += Tests.Argument("-oD")
