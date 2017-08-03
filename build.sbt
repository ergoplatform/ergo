import _root_.sbt.Keys._

organization := "org.ergoplatform"

name := "ergo"

version := "0.1.0"

scalaVersion := "2.12.2"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/")

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "iodb" % "0.3.1",
  "org.scorexfoundation" %% "avl-iodb" % "0.2.0",
  "org.scorexfoundation" %% "scrypto" % "1.2.2" exclude("org.slf4j", "slf4j-api"),
  "org.scorexfoundation" %% "scorex-testkit" % "2.0.0-RC2-SNAPSHOT" % "test",
  "org.scorexfoundation" %% "scorex-core" % "2.0.0-RC2-SNAPSHOT",
  "com.storm-enroute" %% "scalameter" % "0.8.2"
)

javaOptions in run ++= Seq("-Xmx2G")


homepage := Some(url("http://ergoplatform.org/"))

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))




