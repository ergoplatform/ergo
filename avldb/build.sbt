import sbt.Keys.testFrameworks

val scala212 = "2.12.20"
val scala213 = "2.13.18"

name := "avldb"

val Versions = new {

  val spire = (scalaVersion: String) =>
    if (scalaVersion == scala213) "0.17.0-M1"
    else "0.16.2"

  val scalameter = (scalaVersion: String) =>
    if (scalaVersion == scala213) "0.21"
    else "0.19"

  val scalacheck = "1.18.1"

  val scalatestplus =
    "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
}

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.13",
  "com.google.guava" % "guava" % "23.0",
  "org.scorexfoundation" %% "scrypto" % "3.1.1",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.scalacheck" %% "scalacheck" % Versions.scalacheck % Test,
  Versions.scalatestplus,
  "com.storm-enroute" %% "scalameter" % Versions.scalameter(scalaVersion.value) % Test,
  "org.typelevel" %% "spire" % Versions.spire(scalaVersion.value)
)

Test / testOptions := Seq(Tests.Filter(t => !t.matches(".*Benchmark$")))
run / javaOptions += "-Xmx6G"

publishMavenStyle := true

Test / publishArtifact := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

scalacOptions ++= Seq("-feature", "-deprecation")

// set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
// see https://github.com/eclipse/jetty.project/issues/3244
// these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
// see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
Compile / compile / scalacOptions ++= Seq("-release", "8")
scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xfatal-warnings")

enablePlugins(ReproducibleBuildsPlugin)