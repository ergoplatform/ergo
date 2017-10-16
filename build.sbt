import _root_.sbt.Keys._

organization := "org.ergoplatform"

name := "ergo"

version := "0.1.0"

scalaVersion := "2.12.3"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")


libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "org.scorexfoundation" %% "scorex-core" % "2.0.0-RC3",
  "org.scorexfoundation" %% "avl-iodb" % "0.2.11",
  "com.storm-enroute" %% "scalameter" % "0.8.2",
  "com.iheart" %% "ficus" % "1.4.1",

  "org.scalactic" %% "scalactic" % "3.0.1" % "test",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "scorex-testkit" % "2.0.0-RC3-SNAPSHOT" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.9" % "test"
)

javaOptions in run ++= Seq("-Xmx2G")

homepage := Some(url("http://ergoplatform.org/"))

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))