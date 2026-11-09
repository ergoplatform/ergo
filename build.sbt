import sbt.Keys.{licenses, _}
import sbt._

logLevel := Level.Debug

// this values should be in sync with ergo-wallet/build.sbt
val scala212 = "2.12.20"
val scala213 = "2.13.18"

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
    "SonaType" at "https://oss.sonatype.org/content/groups/public",
    "Repo for leveldbjni-all" at "https://gitlab.com/api/v4/projects/61211221/packages/maven",
    "Typesafe maven releases" at "https://dl.bintray.com/typesafe/maven-releases/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"),
  homepage := Some(url("http://ergoplatform.org/")),
  description := "Ergo Platform node and libraries",
  licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode")),
  publishTo := {
    val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
    if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
    else localStaging.value
  },
  scmInfo := Some(
      ScmInfo(
          url("https://github.com/ergoplatform/ergo"),
          "scm:git@github.com:ergoplatform/ergo.git"
      )
  ),
)

publishArtifact in (Compile, packageDoc) := false

val circeVersion = "0.14.16"
val akkaVersion = "2.6.10"
val akkaHttpVersion = "10.2.4"

val sigmaStateVersion = "6.0.7"
val ficusVersion = "1.4.7"

// for testing current sigmastate build (see sigmastate-ergo-it jenkins job)
val effectiveSigmaStateVersion = Option(System.getenv().get("SIGMASTATE_VERSION")).getOrElse(sigmaStateVersion)
val effectiveSigma = "org.scorexfoundation" %% "sigma-state" % effectiveSigmaStateVersion

libraryDependencies ++= Seq(
  effectiveSigma.force()
    .exclude("ch.qos.logback", "logback-classic")
    .exclude("org.scorexfoundation", "scrypto"),

  "ch.qos.logback" % "logback-classic" % "1.3.5",

  // test dependencies
  "org.scala-lang.modules" %% "scala-async" % "1.0.1" % "test",
  "org.scalactic" %% "scalactic" % "3.0.3" % "test",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test,it",
  "org.scalacheck" %% "scalacheck" % "1.14.+" % "test",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,

  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % "test",

  "org.asynchttpclient" % "async-http-client" % "2.6.+" % "test",
  "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.2" % "test",
  "com.github.docker-java" % "docker-java-core" % "3.3.4" % Test,
  "com.github.docker-java" % "docker-java-transport-httpclient5" % "3.3.4" % Test,

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

run / javaOptions ++= opts
scalacOptions ++= Seq("-Xasync")
scalacOptions --= Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xcheckinit")
val scalacOpts = Seq("-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Ywarn-unused:params", "-Xcheckinit")


Compile / sourceGenerators += Def.task {
  val versionFile = (Compile / sourceManaged).value / "org" / "ergoplatform" / "Version.scala"
  
  IO.write(versionFile,
    s"""package org.ergoplatform
       |
       |object Version {
       |  val VersionString = "${version.value}"
       |}
       |""".stripMargin)
  Seq(versionFile)
}

assembly / mainClass := Some("org.ergoplatform.ErgoApp")

assembly / test := {}

assembly / assemblyJarName := s"ergo-${version.value}.jar"

assembly / assemblyMergeStrategy := {
  case "logback.xml" => MergeStrategy.last
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case "reference.conf" => CustomMergeStrategy.concatReversed
  case PathList("org", "bouncycastle", xs @ _*) => MergeStrategy.first
  case PathList("org", "iq80", "leveldb", xs @ _*) => MergeStrategy.first
  case PathList("org", "bouncycastle", xs @ _*) => MergeStrategy.first
  case PathList("javax", "activation", xs @ _*) => MergeStrategy.last
  case PathList("javax", "annotation", xs @ _*) => MergeStrategy.last
  case other => (assembly / assemblyMergeStrategy).value(other)
}

enablePlugins(sbtdocker.DockerPlugin)
enablePlugins(JavaAppPackaging)
enablePlugins(ReproducibleBuildsPlugin)

Universal / mappings += {
  val sampleFile = (Compile / resourceDirectory).value / "samples" / "local.conf.sample"
  sampleFile -> "conf/local.conf"
}

// removes all jar mappings in universal and appends the fat jar
Universal / mappings ++= {
  // universalMappings: Seq[(File,String)]
  val universalMappings = (Universal / mappings).value
  val fatJar = (Compile / assembly).value
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

// Number of fixed test groups for unit tests and for integration tests. The groups of one
// configuration all run at once (see concurrentRestrictions).
val testGroups = 3

// Packs suites into `count` fixed test groups: longest first, each into the group with
// the least time so far.
def packTestGroups(
  count: Int,
  tests: Seq[TestDefinition],
  secondsOf: TestDefinition => Int
): Vector[Seq[TestDefinition]] =
  tests
    .sortBy(t => (-secondsOf(t), t.name))
    .foldLeft(Vector.fill(count)((0, Seq.empty[TestDefinition]))) { (gs, t) =>
      val i             = gs.indexOf(gs.minBy(_._1))
      val (time, group) = gs(i)
      gs.updated(i, (time + secondsOf(t), group :+ t))
    }
    .map(_._2)

Defaults.itSettings
configs(IntegrationTest extend Test)
inConfig(IntegrationTest)(Seq(
  // Run integration suites in parallel, in testGroups fixed groups. Each group is forked
  // into its own JVM and runs its suites in sequence. sbt starts forked groups in a random
  // order. With one group per suite, a long suite could start last and set the job time.
  // The groups are packed by each suite's time on a GitHub-hosted runner, in seconds. A
  // suite that is not listed counts as 30 s.
  parallelExecution := true,
  testGrouping := {
    val opts = forkOptions.value
    val seconds = Map(
      "DeepRollBackSpec"            -> 145,
      "PrunedDigestNodeSyncSpec"    -> 115,
      "ForkResolutionSpec"          -> 62,
      "LongChainSyncSpec"           -> 51,
      "UtxoStateNodesSyncSpec"      -> 45,
      "PrunedDigestNodeSync2Spec"   -> 40,
      "StateRecoveryDigestNodeSpec" -> 33,
      "KnownNodesSpec"              -> 25,
      "NodeRecoverySpec"            -> 18,
      "NodeApiWaitSpec"             -> 11,
      "WalletSpec"                  -> 9,
      "DockerVolumeCleanupSpec"     -> 5,
      "ConvergenceObservationsSpec" -> 1,
      "StallWatchSpec"              -> 1
    )
    def secondsOf(t: TestDefinition): Int = seconds.getOrElse(t.name.split('.').last, 30)

    val groups = packTestGroups(testGroups, definedTests.value, secondsOf)
    groups.filter(_.nonEmpty).zipWithIndex.map {
      case (tests, i) =>
        Tests.Group(name = s"group-$i", tests = tests, runPolicy = Tests.SubProcess(opts))
    }
  },
  test := (test dependsOn docker).value,
  scalacOptions ++= Seq("-Xasync")
))

// Cap how many forked test JVMs run concurrently.  sbt's default
// concurrentRestrictions already contains Tags.limit(Tags.ForkedTestGroup, 1);
// simply += adds a second, looser rule and the strictest wins, so the
// parallelism would be a no-op.  We replace the default rule by
// keeping the other defaults and setting ForkedTestGroup to testGroups. The limit
// applies to the unit test groups and to the integration test groups.
// Three integration suites start at most ~11 node containers of 1 GB each,
// which fits the 16 GB of a GitHub-hosted runner.
Global / concurrentRestrictions := Seq(
  Tags.limitAll(math.max(1, java.lang.Runtime.getRuntime.availableProcessors())),
  Tags.limit(Tags.ForkedTestGroup, testGroups),
  Tags.exclusiveGroup(Tags.Clean)
)

docker / dockerfile := {
  val configDevNet = (IntegrationTest / resourceDirectory).value / "devnetTemplate.conf"
  val configTestNet = (IntegrationTest / resourceDirectory).value / "testnetTemplate.conf"
  val configMainNet = (IntegrationTest / resourceDirectory).value / "mainnetTemplate.conf"

  new Dockerfile {
    from("eclipse-temurin:11-jre-jammy")
    label("ergo-integration-tests", "ergo-integration-tests")
    add(assembly.value, "/opt/ergo/ergo.jar")
    add(Seq(configDevNet), "/opt/ergo")
    add(Seq(configTestNet), "/opt/ergo")
    add(Seq(configMainNet), "/opt/ergo")
  }
}

docker / buildOptions := BuildOptions(
  removeIntermediateContainers = BuildOptions.Remove.OnSuccess
)

//Scapegoat settings

ThisBuild / scapegoatVersion := "1.3.11"

scapegoatDisabledInspections := Seq("FinalModifierOnCaseClass")

Test / testOptions := Seq(Tests.Filter(s => !s.endsWith("Bench")))

// Run the unit suites in testGroups fixed groups, packed like the integration suites. The
// times are seconds on a GitHub-hosted runner. A suite that is not listed counts as 1 s.
// The keys are full names, because two suites are called DeliveryTrackerSpec.
// Each group but the first gets its own default data directory, so that two JVMs never
// open the same database there. ErgoSettingsSpecification checks the default directory,
// so its group comes first.
Test / parallelExecution := true
Test / testGrouping := {
  val forkOpts = (Test / forkOptions).value
  val base     = baseDirectory.value
  val seconds = Map(
    "org.ergoplatform.network.ErgoNodeViewSynchronizerSpecification"            -> 124,
    "org.ergoplatform.nodeView.viewholder.ErgoNodeViewHolderSpec"               -> 109,
    "org.ergoplatform.mining.CandidateGeneratorSpec"                            -> 51,
    "org.ergoplatform.mining.ErgoMinerSpec"                                     -> 42,
    "org.ergoplatform.nodeView.history.extra.ExtraIndexerSpecification"         -> 38,
    "org.ergoplatform.nodeView.history.extra.ExtraIndexerAppliedBlocksSpec"     -> 24,
    "org.ergoplatform.settings.ErgoSettingsSpecification"                       -> 23,
    "org.ergoplatform.network.DeliveryTrackerSpec"                              -> 22,
    "org.ergoplatform.nodeView.state.UtxoStateSpecification"                    -> 21,
    "org.ergoplatform.nodeView.viewholder.PrunedNodeViewHolderSpec"             -> 15,
    "org.ergoplatform.nodeView.history.UtxoBootstrapNoNipopowSyncSpecification" -> 15,
    "org.ergoplatform.nodeView.history.extra.RestoreStateFailureWitnessSpec"    -> 14,
    "org.ergoplatform.nodeView.history.PopowProcessorSpecification"             -> 11,
    "org.ergoplatform.nodeView.state.ErgoStateSpecification"                    -> 11,
    "org.ergoplatform.mining.CandidateGeneratorStorageRentSpec"                 -> 8,
    "org.ergoplatform.nodeView.state.DigestStateSpecification"                  -> 7,
    "org.ergoplatform.nodeView.history.extra.ExtraIndexerReorgAddressTxsSpec"   -> 6,
    "org.ergoplatform.modifiers.mempool.ErgoNodeTransactionSpec"                -> 6,
    "org.ergoplatform.mining.CandidateGeneratorPropSpec"                        -> 5,
    "org.ergoplatform.modifiers.history.PoPowAlgosWithDBSpec"                   -> 4
  )
  def secondsOf(t: TestDefinition): Int = seconds.getOrElse(t.name, 1)

  val defaultDirSuite = "org.ergoplatform.settings.ErgoSettingsSpecification"
  val packed          = packTestGroups(testGroups, (Test / definedTests).value, secondsOf)
  val groups          = packed.sortBy(group => !group.exists(_.name == defaultDirSuite))
  groups.zipWithIndex.collect {
    case (tests, i) if tests.nonEmpty =>
      val dataDir = base / ".ergo_test" / s"group-$i" / "data"
      val jvmOptions =
        if (i == 0) forkOpts.runJVMOptions
        else forkOpts.runJVMOptions :+ s"-Dergo.directory=$dataDir"
      val policy = Tests.SubProcess(forkOpts.withRunJVMOptions(jvmOptions))
      Tests.Group(name = s"group-$i", tests = tests, runPolicy = policy)
  }
}

lazy val avldb = (project in file("avldb"))
  .disablePlugins(ScapegoatSbtPlugin) // not compatible with crossScalaVersions
  .settings(
    crossScalaVersions := Seq(scala213, scalaVersion.value),
    commonSettings,
    name := "avldb",
    // set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
    // see https://github.com/eclipse/jetty.project/issues/3244
    // these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
    // see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
    Compile / compile / scalacOptions ++= Seq("-release", "8"),
    Compile / compile / scalacOptions --= scalacOpts,
    Compile / compile / javacOptions ++= javacReleaseOption,
    libraryDependencies ++= Seq(
      // database dependencies
      "org.ethereum" % "leveldbjni-all" % "1.18.3",
      //the following pure-java leveldb implementation is needed only on specific platforms, such as 32-bit Raspberry Pi
      //in future, it could be reasonable to have special builds with this Java db only, and for most of platforms use
      //jni wrapper over native library included in leveldbjni-all
      "org.iq80.leveldb" % "leveldb" % "0.12"
    )
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
    Test / parallelExecution := false,
    logBuffered := false
  )
  .dependsOn(avldb)
  .enablePlugins(JmhPlugin)

lazy val ergoCore = (project in file("ergo-core"))
  .disablePlugins(ScapegoatSbtPlugin) // not compatible with crossScalaVersions
  .dependsOn(avldb % "test->test;compile->compile")
  .dependsOn(ergoWallet % "test->test;compile->compile")
  .settings(
    crossScalaVersions := Seq(scala213, scalaVersion.value),
    commonSettings,
    name := "ergo-core",
    libraryDependencies ++= Seq(
      "com.iheart" %% "ficus" % ficusVersion,
      effectiveSigma,
      (effectiveSigma % Test).classifier("tests")
    ),
    Compile / compile / scalacOptions ++= Seq("-release", "8"),
    Compile / compile / scalacOptions --= scalacOpts,
    Test / parallelExecution := false,
  )

lazy val ergoWallet = (project in file("ergo-wallet"))
  .disablePlugins(ScapegoatSbtPlugin) // not compatible with crossScalaVersions
  .settings(
    crossScalaVersions := Seq(scala213, scalaVersion.value),
    commonSettings,
    name := "ergo-wallet",
    libraryDependencies ++= Seq(
      effectiveSigma,
      (effectiveSigma % Test).classifier("tests")
    ),
    Compile / compile / scalacOptions ++= Seq("-release", "8"),
  )

lazy val It2Test = config("it2") extend (IntegrationTest, Test)
configs(It2Test)
inConfig(It2Test)(Defaults.testSettings ++ Seq(
  parallelExecution := false,
  test := (test dependsOn docker).value,
  scalacOptions ++= Seq("-Xasync")
))

lazy val ergo = (project in file("."))
  .settings(
    commonSettings,
    name := "ergo",
    // set bytecode version to 8 to fix NoSuchMethodError for various ByteBuffer methods
    // see https://github.com/eclipse/jetty.project/issues/3244
    // these options applied only in "compile" task since scalac crashes on scaladoc compilation with "-release 8"
    // see https://github.com/scala/community-builds/issues/796#issuecomment-423395500
    Compile / compile / scalacOptions ++= Seq("-release", "8"),
    Compile / compile / javacOptions ++= javacReleaseOption,
    libraryDependencies ++= Seq(
      // api dependencies
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      // network dependencies
      "com.typesafe.akka" %% "akka-stream" % akkaVersion, // required for akka-http to compile
      "com.typesafe.akka" %% "akka-actor" % akkaVersion, // required for akka-http to compile
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-core" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-parsing" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,

      "org.bitlet" % "weupnp" % "0.1.4",
      // command line args parsing
      "com.github.scopt" %% "scopt" % "4.1.0",

      // API dependencies
      "de.heikoseeberger" %% "akka-http-circe" % "1.39.2",

      // app dependencies
      // jaxb-api is included only to avoid a runtime exception
      "javax.xml.bind" % "jaxb-api" % "2.4.0-b180830.0359",

      // caching
      "com.github.ben-manes.caffeine" % "caffeine" % "2.9.3" // use 3.x only for java 11+
    )
  )
  .dependsOn(ergoCore % "test->test;compile->compile")
  .dependsOn(ergoWallet % "test->test;compile->compile")
  .dependsOn(avldb % "test->test;compile->compile")
  .configs(It2Test)


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
} yield Credentials("Sonatype Nexus Repository Manager", "central.sonatype.com", username, password)).toSeq

def javacReleaseOption = {
  if (System.getProperty("java.version").startsWith("1.")) 
    // java <9 "--release" is not supported
    Seq()
  else
    Seq("--release", "8")
}

// prefix version with "-SNAPSHOT" for builds without a git tag
ThisBuild / dynverSonatypeSnapshots := true
// use "-" instead of default "+"
ThisBuild / dynverSeparator := "-"
