import sbt.Keys.testFrameworks

name := "avldb"

libraryDependencies ++= Seq(
  "javax.xml.bind" % "jaxb-api" % "2.4.0-b180830.0359",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scorexfoundation" %% "scrypto" % "2.1.10"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
  "com.storm-enroute" %% "scalameter" % "0.9" % "test"
)

libraryDependencies ++= Seq(
  "org.ethereum" % "leveldbjni-all"     % "1.18.3"
)

testOptions in Test := Seq(Tests.Filter(t => !t.matches(".*Benchmark$")))
javaOptions in run += "-Xmx12G"

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

// set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
// see https://github.com/eclipse/jetty.project/issues/3244
// these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
// see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
scalacOptions in(Compile, compile) ++= Seq("-release", "8")
