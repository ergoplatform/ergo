import sbt.Keys.testFrameworks

val scala211 = "2.11.12"
val scala212 = "2.12.18"
val scala213 = "2.13.12"

name := "avldb"

val Versions = new {

  val spire = (scalaVersion: String) =>
    if (scalaVersion == scala213) "0.17.0"
    else "0.14.1"

  val scalameter = (scalaVersion: String) =>
    if (scalaVersion == scala213) "0.19"
    else "0.9"
}

libraryDependencies ++= Seq(
  "javax.xml.bind" % "jaxb-api" % "2.4.0-b180830.0359",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.google.guava" % "guava" % "23.0",
  "org.scorexfoundation" %% "scrypto" % "2.3.0",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
  "com.storm-enroute" %% "scalameter" % Versions.scalameter(scalaVersion.value) % "test",
  "org.rocksdb" % "rocksdbjni" % "8.9.1",
  "org.typelevel" %% "spire" % Versions.spire(scalaVersion.value)
)

testOptions in Test := Seq(Tests.Filter(t => !t.matches(".*Benchmark$")))
javaOptions in run += "-Xmx6G"

publishMavenStyle := true

publishArtifact in Test := false

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
scalacOptions in(Compile, compile) ++= (if (scalaBinaryVersion.value == "2.11") Seq() else Seq("-release", "8"))
scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xfatal-warnings")

enablePlugins(ReproducibleBuildsPlugin)