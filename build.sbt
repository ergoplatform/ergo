import sbt.Keys._
import sbt._

organization := "org.ergoplatform"

name := "ergo"

version := "1.0.2-SNAPSHOT"

scalaVersion := "2.12.6"

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")

val scorexVersion = "b2835a7b-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scrypto" % "2.1.2",
  "org.scorexfoundation" %% "sigma-state" % "0.9.3",
  "org.scala-lang.modules" %% "scala-async" % "0.9.7",
  "org.scorexfoundation" %% "avl-iodb" % "0.2.14",
  "org.scorexfoundation" %% "iodb" % "0.3.2",
  ("org.scorexfoundation" %% "scorex-core" % scorexVersion).exclude("ch.qos.logback", "logback-classic"),
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "javax.xml.bind" % "jaxb-api" % "2.+",
  "com.iheart" %% "ficus" % "1.4.+",

  "com.storm-enroute" %% "scalameter" % "0.8.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalatest" %% "scalatest" % "3.0.+" % "test,it",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "org.scorexfoundation" %% "scorex-testkit" % scorexVersion % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.+" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.+" % "test",
  "org.asynchttpclient" % "async-http-client" % "2.1.0-alpha22" % "test",
  "com.spotify" % "docker-client" % "8.11.0" % "test" classifier "shaded",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.2" % "test"
)

coverageExcludedPackages := ".*ErgoApp.*;.*routes.*;.*ErgoPersistentModifier"

fork := true

val opts = Seq(
  "-server",
  // JVM memory tuning for 2g ram
  "-Xms128m",
  "-Xmx2G",
  //64M for stack, reduce after optimizations
  "-Xss64m",
  "-XX:+ExitOnOutOfMemoryError",
  // Java 9 support
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind",

  // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
  "-XX:+UseG1GC",
  "-XX:+UseNUMA",
  "-XX:+AlwaysPreTouch",

  // probably can't use these with jstack and others tools
  "-XX:+PerfDisableSharedMem",
  "-XX:+ParallelRefProcEnabled",
  "-XX:+UseStringDeduplication")

// todo after adding sbt-native-packager
//javaOptions in Universal ++= opts.map(opt => "-J" + opt)

// -J prefix is required by the bash script
javaOptions in run ++= opts

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
  case "module-info.class" => MergeStrategy.discard
  case other => (assemblyMergeStrategy in assembly).value(other)
}

enablePlugins(sbtdocker.DockerPlugin)

Defaults.itSettings
configs(IntegrationTest extend(Test))
inConfig(IntegrationTest)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
  testOptions += Tests.Filter(_.endsWith("Suite"))
))

dockerfile in docker := {
  val configTemplate = (resourceDirectory in IntegrationTest).value / "template.conf"
  val startErgo = (sourceDirectory in IntegrationTest).value / "container" / "start-ergo.sh"

  new Dockerfile {
    from("openjdk:9-jre-slim")
    label("ergo-integration-tests", "ergo-integration-tests")
    add(assembly.value, "/opt/ergo/ergo.jar")
    add(Seq(configTemplate, startErgo), "/opt/ergo/")
    run("chmod", "+x", "/opt/ergo/start-ergo.sh")
    entryPoint("/opt/ergo/start-ergo.sh")
  }
}

buildOptions in docker := BuildOptions(
  removeIntermediateContainers = BuildOptions.Remove.OnSuccess
)


//FindBugs settings

findbugsReportType := Some(FindbugsReport.Xml)
findbugsExcludeFilters := Some(scala.xml.XML.loadFile(baseDirectory.value / "findbugs-exclude.xml"))

//Scapegoat settings

scapegoatVersion in ThisBuild := "1.3.3"

scapegoatDisabledInspections := Seq("FinalModifierOnCaseClass")

val Bench = config("bench") extend Test

inConfig(Bench)(Defaults.testSettings ++  Seq(
  fork in run := true,
  classDirectory := (classDirectory in Compile).value,
  dependencyClasspath := (dependencyClasspath in Compile).value
))

compile in Bench := (compile in Bench).dependsOn(compile in Test).value
