// this values should be in sync with root (i.e. ../build.sbt)
val scala211 = "2.11.12"
val scala212 = "2.12.18"
val scala213 = "2.13.12"

val deps211 = Seq(
  "io.circe" %% "circe-core" % "0.10.0",
  "io.circe" %% "circe-generic" % "0.10.0",
  "io.circe" %% "circe-parser" % "0.10.0")
val deps212 = Seq(
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",
  "io.circe" %% "circe-parser" % "0.13.0")

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-bits" % "1.1.34",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
) ++ (if (scalaVersion.value == scala211) deps211 else deps212)

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

pomExtra in ThisBuild :=
  <developers>
    <developer>
      <id>Oskin1</id>
      <name>Ilya Oskin</name>
    </developer>
  </developers>

// set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
// see https://github.com/eclipse/jetty.project/issues/3244
// these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
// see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
scalacOptions in(Compile, compile) ++= (if (scalaBinaryVersion.value == "2.11") Seq() else Seq("-release", "8"))
scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-unused:imports", "-Ywarn-value-discard", "-Xfatal-warnings")