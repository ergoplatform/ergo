import sbt.Keys.testFrameworks

name := "avldb"

libraryDependencies ++= Seq(
  "javax.xml.bind" % "jaxb-api" % "2.+",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scorexfoundation" %% "scrypto" % "2.1.4",
  "org.scorexfoundation" %% "iodb" % "0.3.2"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test",
  "com.storm-enroute" %% "scalameter" % "0.9" % "test"
)

libraryDependencies ++= Seq(
  ("org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8").exclude("org.iq80.leveldb", "leveldb"),
  "org.iq80.leveldb" % "leveldb" % "0.12"
)

testOptions in Test := Seq(Tests.Filter(t => !t.matches(".*Benchmark$")))

//scalacOptions ++= Seq("-Xdisable-assertions")

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

scalacOptions ++= Seq("-Xfatal-warnings", "-feature", "-deprecation")
