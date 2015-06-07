name := "scaped"

scalaVersion := "2.11.6"

version := "0.0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.0",
  "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"
)
