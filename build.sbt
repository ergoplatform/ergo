import _root_.sbt.Keys._

organization := "org.ergoplatform"

name := "ergo"

version := "0.1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scorex-core" % "2.0.0-M5-SNAPSHOT"
)

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage := Some(url("http://ergoplatform.org/"))

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

javaOptions in run ++= Seq("-Xmx1G")



