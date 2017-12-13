import sbt.Keys._
import sbt._

organization := "org.ergoplatform"

name := "ergo"

version := "0.1.1"

scalaVersion := "2.12.3"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

val scorexVersion = "2.0.0-RC3-284-g1054a3d-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  "org.scorexfoundation" %% "scorex-core" % scorexVersion,
  "org.scorexfoundation" %% "avl-iodb" % "0.2.11",
  "com.storm-enroute" %% "scalameter" % "0.8.+",
  "com.iheart" %% "ficus" % "1.4.+",

  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalatest" %% "scalatest" % "3.0.+" % "test,it",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "scorex-testkit" % scorexVersion % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.9" % "test",
  "org.asynchttpclient" % "async-http-client" % "2.1.0-alpha22" % "it",
  "com.spotify" % "docker-client" % "8.8.2" % "it" classifier "shaded",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.2" % "it"
)

coverageExcludedPackages := ".*ErgoApp.*;.*routes.*;.*ErgoPersistentModifier"

fork := true

javaOptions in run ++= Seq(
  // -J prefix is required by the bash script
  "-J-server",
  // JVM memory tuning for 2g ram
  "-J-Xms128m",
  "-J-Xmx2G",
  "-J-XX:+ExitOnOutOfMemoryError",
  // Java 9 support
  "-J-XX:+IgnoreUnrecognizedVMOptions",
  "-J--add-modules=java.xml.bind",

  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-J-XX:+UseG1GC",
  "-J-XX:+UseNUMA",
  "-J-XX:+AlwaysPreTouch",

  // probably can't use these with jstack and others tools
  "-J-XX:+PerfDisableSharedMem",
  "-J-XX:+ParallelRefProcEnabled",
  "-J-XX:+UseStringDeduplication")

homepage := Some(url("http://ergoplatform.org/"))

licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

sourceGenerators in Compile += Def.task {
  val versionFile = (sourceManaged in Compile).value / "org" / "ergoplatform" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r
  val versionExtractor(major, minor, bugfix) = version.value
  IO.write(versionFile,
    s"""package org.ergoplatform
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |  val VersionTuple = ($major, $minor, $bugfix)
       |}
       |""".stripMargin)
  Seq(versionFile)
}

mainClass in assembly := Some("org.ergoplatform.ErgoApp")

test in assembly := {}

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case other => (assemblyMergeStrategy in assembly).value(other)
}

enablePlugins(sbtdocker.DockerPlugin)

Defaults.itSettings
configs(IntegrationTest)
inConfig(IntegrationTest)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
  testOptions += Tests.Filter(_.endsWith("Suite"))
))

dockerfile in docker := {
  val configTemplate = (resourceDirectory in IntegrationTest).value / "template.conf"
  val startErgo = (sourceDirectory in IntegrationTest).value / "container" / "start-ergo.sh"

  new Dockerfile {
    from("anapsix/alpine-java:8_server-jre")
    add(assembly.value, "/opt/ergo/ergo.jar")
    add(Seq(configTemplate, startErgo), "/opt/ergo/")
    run("chmod", "+x", "/opt/ergo/start-ergo.sh")
    entryPoint("/opt/ergo/start-ergo.sh")
  }
}

buildOptions in docker := BuildOptions(
  removeIntermediateContainers = BuildOptions.Remove.OnSuccess
)
