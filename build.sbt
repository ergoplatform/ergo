import sbt.Keys.{licenses, _}
import sbt._

logLevel := Level.Debug

// this values should be in sync with ergo-wallet/build.sbt
val scala211 = "2.11.12"
val scala212 = "2.12.10"
val scala213 = "2.13.8"

lazy val commonSettings = Seq(
  organization := "org.ergoplatform",
  name := "ergo",
  scalaVersion := scala212,
  // version is set via git tag vX.Y.Z:
  // $ git tag v3.2.0
  // $ git push origin v3.2.0
  // without the tag version resolves to [branch name]-[git commit hash]-SNAPSHOT
  // don't set the version manually
  resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Bintray" at "https://jcenter.bintray.com/", //for org.ethereum % leveldbjni-all 
    "SonaType" at "https://oss.sonatype.org/content/groups/public",
    "Typesafe maven releases" at "https://dl.bintray.com/typesafe/maven-releases/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"),
  homepage := Some(url("http://ergoplatform.org/")),
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  publishTo := sonatypePublishToBundle.value,
  scmInfo := Some(
      ScmInfo(
          url("https://github.com/ergoplatform/ergo"),
          "scm:git@github.com:ergoplatform/ergo.git"
      )
  ),
)

val circeVersion = "0.13.0"
val akkaVersion = "2.6.10"
val akkaHttpVersion = "10.2.4"

val sigmaStateVersion = "5.0.8"

// for testing current sigmastate build (see sigmastate-ergo-it jenkins job)
val effectiveSigmaStateVersion = Option(System.getenv().get("SIGMASTATE_VERSION")).getOrElse(sigmaStateVersion)
val effectiveSigma = "org.scorexfoundation" %% "sigma-state" % effectiveSigmaStateVersion

val apiDependencies = Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.0"
)

libraryDependencies ++= Seq(
  effectiveSigma.force()
    .exclude("ch.qos.logback", "logback-classic")
    .exclude("org.scorexfoundation", "scrypto"),

  // network dependencies 
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-parsing" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "org.bitlet" % "weupnp" % "0.1.4",
  
  // api dependencies 
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "de.heikoseeberger" %% "akka-http-circe" % "1.20.0",
  
  "org.ethereum" % "leveldbjni-all" % "1.18.3",
  //the following pure-java leveldb implementation is needed only on specific platforms, such as 32-bit Raspberry Pi
  //in future, it could be reasonable to have special builds with this Java db only, and for most of platforms use
  //jni wrapper over native library included in leveldbjni-all
  "org.iq80.leveldb" % "leveldb" % "0.12",
  
  "javax.xml.bind" % "jaxb-api" % "2.4.0-b180830.0359",
  "com.iheart" %% "ficus" % "1.4.7",
  "ch.qos.logback" % "logback-classic" % "1.3.5",
  "com.google.guava" % "guava" % "21.0",
  "com.github.ben-manes.caffeine" % "caffeine" % "2.9.3", // use 3.x only for java 11+
  "com.github.scopt" %% "scopt" % "4.0.1",

  "org.scala-lang.modules" %% "scala-async" % "0.9.7" % "test",
  "com.storm-enroute" %% "scalameter" % "0.8.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.3" % "test",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test,it",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,

  "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-core" % circeVersion % "test",

  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test",

  "org.asynchttpclient" % "async-http-client" % "2.6.+" % "test",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.2" % "test",
  "com.spotify" % "docker-client" % "8.14.5" % "test" classifier "shaded"
)

updateOptions := updateOptions.value.withLatestSnapshots(false)

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
  "-XX:+UseStringDeduplication"
)

javaOptions in run ++= opts
scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xcheckinit")

sourceGenerators in Compile += Def.task {
  val versionFile = (sourceManaged in Compile).value / "org" / "ergoplatform" / "Version.scala"
  
  IO.write(versionFile,
    s"""package org.ergoplatform
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |}
       |""".stripMargin)
  Seq(versionFile)
}

mainClass in assembly := Some("org.ergoplatform.ErgoApp")

test in assembly := {}

assemblyJarName in assembly := s"ergo-${version.value}.jar"

assemblyMergeStrategy in assembly := {
  case "logback.xml" => MergeStrategy.first
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case "reference.conf" => CustomMergeStrategy.concatReversed
  case PathList("org", "bouncycastle", xs @ _*) => MergeStrategy.first
  case PathList("org", "iq80", "leveldb", xs @ _*) => MergeStrategy.first
  case PathList("org", "bouncycastle", xs @ _*) => MergeStrategy.first
  case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
  case PathList("javax", "annotation", xs @ _*) => MergeStrategy.last
  case other => (assemblyMergeStrategy in assembly).value(other)
}

enablePlugins(sbtdocker.DockerPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(ReproducibleBuildsPlugin)

mappings in Universal += {
  val sampleFile = (resourceDirectory in Compile).value / "samples" / "local.conf.sample"
  sampleFile -> "conf/local.conf"
}

// removes all jar mappings in universal and appends the fat jar
mappings in Universal ++= {
  // universalMappings: Seq[(File,String)]
  val universalMappings = (mappings in Universal).value
  val fatJar = (assembly in Compile).value
  // removing means filtering
  val filtered = universalMappings filter {
    case (_, name) => !name.endsWith(".jar")
  }
  // add the fat jar
  filtered :+ (fatJar -> ("lib/" + fatJar.getName))
}

// add jvm parameter for typesafe config
bashScriptExtraDefines += """addJava "-Dconfig.file=${app_home}/../conf/local.conf""""

inConfig(Linux)(
  Seq(
    maintainer := "ergoplatform.org",
    packageSummary := "Ergo node",
    packageDescription := "Ergo node"
  )
)

Defaults.itSettings
configs(IntegrationTest extend Test)
inConfig(IntegrationTest)(Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
))

dockerfile in docker := {
  val configDevNet = (resourceDirectory in IntegrationTest).value / "devnetTemplate.conf"
  val configTestNet = (resourceDirectory in IntegrationTest).value / "testnetTemplate.conf"
  val configMainNet = (resourceDirectory in IntegrationTest).value / "mainnetTemplate.conf"

  new Dockerfile {
    from("openjdk:9-jre-slim")
    label("ergo-integration-tests", "ergo-integration-tests")
    add(assembly.value, "/opt/ergo/ergo.jar")
    add(Seq(configDevNet), "/opt/ergo")
    add(Seq(configTestNet), "/opt/ergo")
    add(Seq(configMainNet), "/opt/ergo")
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

Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("Bench")))

lazy val avldb = (project in file("avldb"))
  .settings(
    commonSettings,
    name := "avldb",
    // set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
    // see https://github.com/eclipse/jetty.project/issues/3244
    // these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
    // see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
    scalacOptions in(Compile, compile) ++= Seq("-release", "8"),
    javacOptions in(Compile, compile) ++= javacReleaseOption,
  )

lazy val avldb_benchmarks = (project in file("avldb/benchmarks"))
  .settings(
    commonSettings,
    name := "avldb-benchmarks",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.9" % "test"
    ),
    publishArtifact := false,
    resolvers ++= Seq("Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    logBuffered := false
  )
  .dependsOn(avldb)
  .enablePlugins(JmhPlugin)

lazy val ergoWallet = (project in file("ergo-wallet"))
  .disablePlugins(ScapegoatSbtPlugin) // not compatible with crossScalaVersions
  .settings(
    crossScalaVersions := Seq(scala213, scalaVersion.value, scala211),
    commonSettings,
    name := "ergo-wallet",
    libraryDependencies ++= Seq(
      effectiveSigma,
      (effectiveSigma % Test).classifier("tests")
    ),
    scalacOptions in(Compile, compile) ++= (if(scalaBinaryVersion.value == "2.11")
        Seq.empty
      else
        Seq("-release", "8")
      ),
  )

lazy val It2Test = config("it2") extend (IntegrationTest, Test)
configs(It2Test)
inConfig(It2Test)(Defaults.testSettings ++ Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
))

lazy val ergo = (project in file("."))
  .settings(
    commonSettings, 
    name := "ergo",
    // set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
    // see https://github.com/eclipse/jetty.project/issues/3244
    // these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
    // see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
    scalacOptions in(Compile, compile) ++= Seq("-release", "8"),
    javacOptions in(Compile, compile) ++= javacReleaseOption
  )
  .dependsOn(ergoWallet % "test->test;compile->compile")
  .dependsOn(avldb % "test->test;compile->compile")
  .configs(It2Test)

lazy val benchmarks = (project in file("benchmarks"))
  .settings(commonSettings, name := "ergo-benchmarks")
  .dependsOn(ergo % "test->test")
  .enablePlugins(JmhPlugin)

// PGP key for signing a release build published to sonatype
// signing is done by sbt-pgp plugin
// how to generate a key - https://central.sonatype.org/pages/working-with-pgp-signatures.html
// how to export a key and use it with Travis - https://docs.scala-lang.org/overviews/contributors/index.html#export-your-pgp-key-pair
pgpPublicRing := file("ci/pubring.asc")
pgpSecretRing := file("ci/secring.asc")
pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toArray)
usePgpKeyHex("D78982639AD538EF361DEC6BF264D529385A0333")

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

def javacReleaseOption = {
  if (System.getProperty("java.version").startsWith("1.")) 
    // java <9 "--release" is not supported
    Seq()
  else
    Seq("--release", "8")
}

// prefix version with "-SNAPSHOT" for builds without a git tag
dynverSonatypeSnapshots in ThisBuild := true
// use "-" instead of default "+"
dynverSeparator in ThisBuild := "-"
