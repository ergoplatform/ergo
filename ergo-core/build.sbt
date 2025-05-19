// this values should be in sync with root (i.e. ../build.sbt)
val scala211 = "2.11.12"
val scala212 = "2.12.20"
val scala213 = "2.13.16"

val deps211 = Seq(
  "io.circe" %% "circe-core" % "0.10.0",
  "io.circe" %% "circe-generic" % "0.10.0",
  "io.circe" %% "circe-parser" % "0.10.0")
val deps212 = Seq(
  "io.circe" %% "circe-core" % "0.13.0",
  "io.circe" %% "circe-generic" % "0.13.0",
  "io.circe" %% "circe-parser" % "0.13.0")

publishMavenStyle := true
Test / publishArtifact := false

libraryDependencies ++= Seq() ++
  (if (scalaVersion.value == scala211) deps211 else deps212)

scalacOptions ++= (if (scalaBinaryVersion.value == scala211) Seq("-language:implicitConversions") else Seq())
scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xfatal-warnings")