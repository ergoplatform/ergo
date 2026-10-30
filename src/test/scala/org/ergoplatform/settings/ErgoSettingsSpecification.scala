package org.ergoplatform.settings

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest

import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration._
import scala.sys.process.{Process, ProcessLogger}

class ErgoSettingsSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  val initSettings: ErgoSettings = settings

  private val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
  private val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize

  private def runBootstrapProbe(stateType: String,
                                utxoBootstrap: Boolean,
                                verifyTransactions: Boolean,
                                nipopowBootstrap: Boolean,
                                mining: Boolean = false,
                                votingIds: String = ""): (Int, String) = {
    val javaExecutable = Paths
      .get(
        System.getProperty("java.home"),
        "bin",
        if (scala.util.Properties.isWin) "java.exe" else "java")
      .toString
    val output = new java.lang.StringBuffer
    val processLogger =
      ProcessLogger(line => { output.append(line).append(System.lineSeparator()); () })
    val exitCode = Process(Seq(
      javaExecutable,
      "-cp",
      System.getProperty("java.class.path"),
      "org.ergoplatform.settings.ErgoSettingsReaderBootstrapProbe",
      stateType,
      utxoBootstrap.toString,
      verifyTransactions.toString,
      nipopowBootstrap.toString,
      mining.toString,
      votingIds
    )).!(processLogger)
    exitCode -> output.toString
  }

  property("Digest state rejects UTXO snapshot bootstrap in both transaction verification modes") {
    for {
      verifyTransactions <- Seq(false, true)
      nipopowBootstrap <- Seq(false, true)
    } {
      val (exitCode, output) = runBootstrapProbe(
        "digest", true, verifyTransactions, nipopowBootstrap)

      withClue(s"verifyTransactions=$verifyTransactions, nipopowBootstrap=$nipopowBootstrap: $output") {
        exitCode shouldBe 1
        output should include("UTXO snapshot bootstrap requires node.stateType = utxo")
        output should not include "SETTINGS_ACCEPTED"
      }
    }
  }

  property("supported Digest and UTXO bootstrap settings remain valid with and without NiPoPoW") {
    val supportedModes = Seq(
      ("digest", false, false),
      ("digest", false, true),
      ("utxo", false, true),
      ("utxo", true, true))
    for {
      (stateType, utxoBootstrap, verifyTransactions) <- supportedModes
      nipopowBootstrap <- Seq(false, true)
    } {
      val (exitCode, output) = runBootstrapProbe(
        stateType, utxoBootstrap, verifyTransactions, nipopowBootstrap)

      withClue(s"stateType=$stateType, utxoBootstrap=$utxoBootstrap, " +
        s"verifyTransactions=$verifyTransactions, nipopowBootstrap=$nipopowBootstrap: $output") {
        exitCode shouldBe 0
        output should include("SETTINGS_ACCEPTED")
      }
    }
  }

  property("should keep data user home  by default") {
    val settings = ErgoSettingsReader.read()
    settings.directory shouldBe System.getProperty("user.dir") + "/.ergo_test/data"
  }

  property("should read default settings") {
    val settings = ErgoSettingsReader.read()
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      1000,
      utxoSettings = UtxoSettings(false, 0, 2),
      nipopowSettings = NipopowSettings(false, 1),
      mining = true,
      txCostLimit,
      txSizeLimit,
      useExternalMiner                          = false,
      blockCandidateGenerationInterval          = 60.seconds,
      internalMinersCount                       = 1,
      internalMinerPollingInterval              = 1.second,
      miningPubKeyHex                           = None,
      offlineGeneration                         = false,
      keepVersions                              = 200,
      acceptableChainUpdateDelay                = 30.minutes,
      mempoolCapacity                           = 100000,
      mempoolCleanupDuration                    = 10.seconds,
      mempoolSorting                            = SortingOption.FeePerByte,
      rebroadcastCount                          = 3,
      minimalFeeAmount                          = 0,
      headerChainDiff                           = 100,
      adProofsSuffixLength                      = 112*1024,
      extraIndex                                = false
    )
    settings.cacheSettings shouldBe CacheSettings(
      HistoryCacheSettings(
        12, 1000, 100, 1000
      ),
      NetworkCacheSettings(
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      ),
      MempoolCacheSettings(
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      )
    )
    settings.scorexSettings.restApi shouldBe RESTApiSettings(
      bindAddress = new InetSocketAddress("0.0.0.0", 9052),
      apiKeyHash = None,
      corsAllowedOrigin = Some("*"),
      timeout = 5.seconds,
      publicUrl = Some(URI.create("https://example.com:80").toURL)
    )
  }

  property("should read user settings from json file") {
    val settings = ErgoSettingsReader.read(Args(Some("src/test/resources/settings.json"), None))
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      12,
      utxoSettings = UtxoSettings(false, 0, 2),
      nipopowSettings = NipopowSettings(false, 1),
      mining = true,
      txCostLimit,
      txSizeLimit,
      useExternalMiner                          = false,
      blockCandidateGenerationInterval          = 60.seconds,
      internalMinersCount                       = 1,
      internalMinerPollingInterval              = 1.second,
      miningPubKeyHex                           = None,
      offlineGeneration                         = false,
      keepVersions                              = 200,
      acceptableChainUpdateDelay                = 30.minutes,
      mempoolCapacity                           = 100000,
      mempoolCleanupDuration                    = 10.seconds,
      mempoolSorting                            = SortingOption.FeePerByte,
      rebroadcastCount                          = 3,
      minimalFeeAmount                          = 0,
      headerChainDiff                           = 100,
      adProofsSuffixLength                      = 112*1024,
      extraIndex                                = false
    )
    settings.cacheSettings shouldBe CacheSettings(
      HistoryCacheSettings(
        12, 1000, 100, 1000
      ),
      NetworkCacheSettings(
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      ),
      MempoolCacheSettings(
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      )
    )
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettingsReader.read(Args(Some("src/test/resources/settings.conf"), None))
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      13,
      utxoSettings = UtxoSettings(false, 0, 2),
      nipopowSettings = NipopowSettings(false, 1),
      mining = true,
      txCostLimit,
      txSizeLimit,
      useExternalMiner                          = false,
      blockCandidateGenerationInterval          = 60.seconds,
      internalMinersCount                       = 1,
      internalMinerPollingInterval              = 1.second,
      miningPubKeyHex                           = None,
      offlineGeneration                         = false,
      keepVersions                              = 200,
      acceptableChainUpdateDelay                = 30.minutes,
      mempoolCapacity                           = 100000,
      mempoolCleanupDuration                    = 10.seconds,
      mempoolSorting                            = SortingOption.FeePerByte,
      rebroadcastCount                          = 3,
      minimalFeeAmount                          = 0,
      headerChainDiff                           = 100,
      adProofsSuffixLength                      = 112*1024,
      extraIndex                                = false
    )
    settings.cacheSettings shouldBe CacheSettings(
      HistoryCacheSettings(
        12, 1000, 100, 1000
      ),
      NetworkCacheSettings(
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      ),
      MempoolCacheSettings(
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      )
    )
  }

  property("scorex.restApi.publicUrl should be valid") {
    val invalidUrls =
      List(
        "http:invalid",
        "http://localhost",
        "http://127.0.0.1",
        "http://0.0.0.0",
        "http://example.com/foo/bar",
        "http://example.com?foo=bar"
      ).map(s => URI.create(s).toURL)

    invalidUrls.forall(ErgoSettingsReader.invalidRestApiUrl) shouldBe true

    val validUrls =
      List(
        "http://example.com",
        "http://example.com:80",
        "http://82.90.21.31",
        "http://82.90.21.31:80"
      ).map(s => URI.create(s).toURL)

    validUrls.forall(url => !ErgoSettingsReader.invalidRestApiUrl(url)) shouldBe true
  }

  property("localOnly config key should fallback to allowLocal") {
    val baseConfig = ConfigFactory.parseString(
      """
        |scorex {
        |  dataDir = "/tmp/scorex"
        |  logDir = "/tmp/scorex/log"
        |  logging {
        |    level = "INFO"
        |  }
        |  network {
        |    nodeName = "test-node"
        |    bindAddress = "0.0.0.0:9020"
        |    appVersion = "6.0.3"
        |    agentName = "test"
        |    magicBytes = [2, 2, 2, 2]
        |    maxConnections = 30
        |    connectionTimeout = 1s
        |    declaredAddress = "127.0.0.1:9020"
        |    handshakeTimeout = 30s
        |    deliveryTimeout = 10s
        |    maxDeliveryChecks = 100
        |    desiredInvObjects = 400
        |    syncInterval = 5s
        |    syncStatusRefresh = 60s
        |    syncIntervalStable = 30s
        |    syncStatusRefreshStable = 90s
        |    inactiveConnectionDeadline = 10m
        |    syncTimeout = 10s
        |    controllerTimeout = 5s
        |    maxModifiersCacheSize = 1024
        |    getPeersInterval = 2m
        |    maxPeerSpecObjects = 64
        |    temporalBanDuration = 60m
        |    penaltySafeInterval = 2m
        |    penaltyScoreThreshold = 500
        |    peerEvictionInterval = 1h
        |    peerDiscovery = true
        |    knownPeers = []
        |    bannedPeers = []
        |    upnpEnabled = false
        |    localOnly = true
        |  }
        |  restApi {
        |    bindAddress = "0.0.0.0:9052"
        |    apiKeyHash = null
        |    corsAllowedOrigin = "*"
        |    timeout = 5s
        |  }
        |}
      """.stripMargin
    )

    val scorexSettings = ScorexSettings.fromConfig(baseConfig)
    scorexSettings.network.allowLocal shouldBe true
  }

  property("allowLocal should take precedence over localOnly") {
    val baseConfig = ConfigFactory.parseString(
      """
        |scorex {
        |  dataDir = "/tmp/scorex"
        |  logDir = "/tmp/scorex/log"
        |  logging {
        |    level = "INFO"
        |  }
        |  network {
        |    nodeName = "test-node"
        |    bindAddress = "0.0.0.0:9020"
        |    appVersion = "6.0.3"
        |    agentName = "test"
        |    magicBytes = [2, 2, 2, 2]
        |    maxConnections = 30
        |    connectionTimeout = 1s
        |    declaredAddress = "127.0.0.1:9020"
        |    handshakeTimeout = 30s
        |    deliveryTimeout = 10s
        |    maxDeliveryChecks = 100
        |    desiredInvObjects = 400
        |    syncInterval = 5s
        |    syncStatusRefresh = 60s
        |    syncIntervalStable = 30s
        |    syncStatusRefreshStable = 90s
        |    inactiveConnectionDeadline = 10m
        |    syncTimeout = 10s
        |    controllerTimeout = 5s
        |    maxModifiersCacheSize = 1024
        |    getPeersInterval = 2m
        |    maxPeerSpecObjects = 64
        |    temporalBanDuration = 60m
        |    penaltySafeInterval = 2m
        |    penaltyScoreThreshold = 500
        |    peerEvictionInterval = 1h
        |    peerDiscovery = true
        |    knownPeers = []
        |    bannedPeers = []
        |    upnpEnabled = false
        |    localOnly = true
        |    allowLocal = false
        |  }
        |  restApi {
        |    bindAddress = "0.0.0.0:9052"
        |    apiKeyHash = null
        |    corsAllowedOrigin = "*"
        |    timeout = 5s
        |  }
        |}
      """.stripMargin
    )

    val scorexSettings = ScorexSettings.fromConfig(baseConfig)
    scorexSettings.network.allowLocal shouldBe false
  }

  property("genesisId must be 64 lower-case hex characters") {
    val mainnetGenesisId = "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b"
    def readWithGenesisId(id: String): ErgoSettings = ErgoSettingsReader.fromConfig(
      ConfigFactory.parseString(s"""ergo.chain.genesisId = "$id"""").withFallback(ConfigFactory.load()))

    readWithGenesisId(mainnetGenesisId).chainSettings.genesisId shouldBe Some(mainnetGenesisId)

    val malformedIds = Seq("", mainnetGenesisId.toUpperCase, mainnetGenesisId.dropRight(1), mainnetGenesisId + "0",
      mainnetGenesisId.dropRight(1) + "g")
    malformedIds.foreach { id =>
      withClue(s"genesisId = '$id': ") {
        val error = the[Error] thrownBy readWithGenesisId(id)
        error.getMessage should include(s"genesisId = '$id'")
      }
    }
  }

  property("VotingTargets and consistentSettings validate voteable parameter ids when mining") {
    val validConfig = ConfigFactory
      .parseString(
        """
          |ergo.node.mining = true
          |ergo.voting {
          |  1 = 1000000
          |  9 = 40
          |  120 = 1
          |  "rulesToDisable" = [215, 409]
          |}
        """.stripMargin
      )
      .withFallback(ConfigFactory.load())

    val targets = ErgoSettingsReader.fromConfig(validConfig).votingTargets
    targets.targets shouldBe Map[Byte, Int](
      Parameters.StorageFeeFactorIncrease -> 1000000,
      Parameters.SubblocksPerBlockIncrease -> 40,
      Parameters.SoftFork -> 1
    )

    val nonMiningBadConfig = ConfigFactory
      .parseString(
        """
          |ergo.node.mining = false
          |ergo.voting {
          |  "-1" = 1
          |  "0" = 1
          |  "10" = 1
          |  "99" = 1
          |  "123" = 1
          |}
        """.stripMargin
      )
      .withFallback(ConfigFactory.load())
    (ErgoSettingsReader.fromConfig(nonMiningBadConfig).votingTargets.targets.keySet --
      VotingTargets.VoteableParameterIds) shouldBe Set[Byte](-1, 0, 10, 99, 123)

    val (exitCode, output) = runBootstrapProbe(
      "utxo", utxoBootstrap = false, verifyTransactions = true, nipopowBootstrap = false,
      mining = true, votingIds = "-1,0,10,99,123")
    exitCode shouldBe 1
    output should include("Unsupported voting target parameter id(s): -1, 0, 10, 99, 123")
  }

  property("ErgoApp.main logs startup config errors to ergo.log before exiting") {
    val javaExecutable = Paths
      .get(
        System.getProperty("java.home"),
        "bin",
        if (scala.util.Properties.isWin) "java.exe" else "java")
      .toString

    val tempRoot = Files.createTempDirectory("ergo-app-startup-log-")
    try {
      val missingSecretDir = tempRoot.resolve("missing-secret-dir").toString.replace("\\", "/")
      val cases = Seq(
        ("""ergo.node.keepVersions = "not-an-int"""", "keepVersions"),
        ("""ergo.chain.genesisId = "invalid"""", "genesisId = 'invalid'"),
        (s"""ergo.wallet.secretStorage.secretDir = "$missingSecretDir"""", "does not exist or not readable")
      )

      cases.zipWithIndex.foreach { case ((configHocon, expectedLogFragment), idx) =>
        val tempDir = Files.createDirectories(tempRoot.resolve(s"case-$idx"))
        val dataDir = Files.createDirectories(tempDir.resolve("data"))
        val secretDir = Files.createDirectories(dataDir.resolve("wallet/keystore"))
        val confPath = tempDir.resolve("user.conf")
        val fullHocon =
          s"""ergo.directory = "${dataDir.toString.replace("\\", "/")}"
             |ergo.wallet.secretStorage.secretDir = "${secretDir.toString.replace("\\", "/")}"
             |$configHocon
             |""".stripMargin
        Files.write(confPath, fullHocon.getBytes(StandardCharsets.UTF_8))

        val output = new java.lang.StringBuffer
        val processLogger =
          ProcessLogger(line => { output.append(line).append(System.lineSeparator()); () })
        val proc = Process(
          Seq(
            javaExecutable,
            "-Dlogback.configurationFile=logback.xml",
            "-cp",
            System.getProperty("java.class.path"),
            "org.ergoplatform.ErgoApp",
            "-c",
            confPath.toString
          ),
          tempDir.toFile
        ).run(processLogger)
        val exitCode = try {
          Await.result(Future(blocking(proc.exitValue()))(ExecutionContext.global), 30.seconds)
        } finally {
          proc.destroy()
        }

        val logPath = tempDir.resolve("ergo.log")
        withClue(s"config=$configHocon, processOutput=$output") {
          exitCode shouldBe 1
          Files.exists(logPath) shouldBe true
          val logContent = new String(Files.readAllBytes(logPath), StandardCharsets.UTF_8)
          logContent should include(expectedLogFragment)
        }
      }
    } finally {
      Files.walk(tempRoot)
        .sorted(java.util.Comparator.reverseOrder())
        .forEach(p => { p.toFile.delete(); () })
    }
  }

}

object ErgoSettingsReaderBootstrapProbe {
  def main(args: Array[String]): Unit = {
    val stateType = args(0)
    val utxoBootstrap = args(1).toBoolean
    val verifyTransactions = args(2).toBoolean
    val nipopowBootstrap = args(3).toBoolean
    val mining = args.lift(4).exists(_.toBoolean)
    val votingIds = args.lift(5).toSeq.flatMap(_.split(",").filter(_.nonEmpty))
    val baseConfig = ConfigFactory
      .load()
      .withValue("ergo.node.stateType", ConfigValueFactory.fromAnyRef(stateType))
      .withValue("ergo.node.mining", ConfigValueFactory.fromAnyRef(Boolean.box(mining)))
      .withValue("ergo.node.extraIndex", ConfigValueFactory.fromAnyRef(Boolean.box(false)))
      .withValue("ergo.node.blocksToKeep",
        ConfigValueFactory.fromAnyRef(Int.box(if (verifyTransactions) 1000 else 0)))
      .withValue("ergo.node.verifyTransactions", ConfigValueFactory.fromAnyRef(Boolean.box(verifyTransactions)))
      .withValue("ergo.node.utxo.utxoBootstrap", ConfigValueFactory.fromAnyRef(Boolean.box(utxoBootstrap)))
      .withValue("ergo.node.nipopow.nipopowBootstrap", ConfigValueFactory.fromAnyRef(Boolean.box(nipopowBootstrap)))
      .withValue("ergo.chain.genesisId", ConfigValueFactory.fromAnyRef("0" * 64))
    val config = votingIds.foldLeft(baseConfig) { (cfg, id) =>
      cfg.withValue(s"""ergo.voting."$id"""", ConfigValueFactory.fromAnyRef(Int.box(1)))
    }

    val settings = ErgoSettingsReader.fromConfig(config)
    require(settings.nodeSettings.stateType == (if (stateType == "digest") StateType.Digest else StateType.Utxo))
    require(settings.nodeSettings.utxoSettings.utxoBootstrap == utxoBootstrap)
    require(settings.nodeSettings.verifyTransactions == verifyTransactions)
    require(settings.nodeSettings.nipopowSettings.nipopowBootstrap == nipopowBootstrap)
    println("SETTINGS_ACCEPTED")
  }
}
