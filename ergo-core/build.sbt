// this values should be in sync with root (i.e. ../build.sbt)
val scala212 = "2.12.20"
val scala213 = "2.13.18"

val circeDeps = Seq(
  "io.circe" %% "circe-core" % "0.14.15",
  "io.circe" %% "circe-generic" % "0.14.15",
  "io.circe" %% "circe-parser" % "0.14.15")

publishMavenStyle := true
Test / publishArtifact := false

libraryDependencies ++= circeDeps

scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xfatal-warnings")