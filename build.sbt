name := """PCDParser"""

version := "1.0"

scalaVersion := "2.10.1"

libraryDependencies += "org.specs2" %% "specs2" % "1.14" % "test"

Seq(
  scalaSource in Test <<= baseDirectory / "test",
  sourceDirectory in Test <<= baseDirectory / "test"
)