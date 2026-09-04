package org.ergoplatform.settings

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest

import java.net.{InetSocketAddress, URI}
import java.nio.file.Paths
import scala.concurrent.duration._
import scala.sys.process.{Process, ProcessLogger}

class ErgoSettingsSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  val initSettings: ErgoSettings = settings

  private val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
  private val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize

  private def runKeepVersionsProbe(utxoBootstrap: Boolean): (Int, String) = {
    val javaExecutable = Paths
      .get(
        System.getProperty("java.home"),
        "bin",
        if (scala.util.Properties.isWin) "java.exe" else "java")
      .toString
    val output = new StringBuilder
    val processLogger = ProcessLogger(
      line => output.append(line).append(System.lineSeparator()),
      line => output.append(line).append(System.lineSeparator()))
    val exitCode = Process(Seq(
      javaExecutable,
      "-cp",
      System.getProperty("java.class.path"),
      "org.ergoplatform.settings.ErgoSettingsReaderKeepVersionsProbe",
      utxoBootstrap.toString
    )).!(processLogger)
    exitCode -> output.toString()
  }

  property("UTXO snapshot bootstrap rejects keepVersions zero with a precise error") {
    val (exitCode, output) = runKeepVersionsProbe(utxoBootstrap = true)

    exitCode should not be 0
    output should include(
      "nodeSettings.keepVersions must be greater than 0 when UTXO snapshot bootstrap is enabled")
  }

  property("keepVersions zero remains valid without UTXO snapshot bootstrap") {
    val (exitCode, output) = runKeepVersionsProbe(utxoBootstrap = false)

    withClue(output) {
      exitCode shouldBe 0
    }
    output should include("SETTINGS_ACCEPTED")
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

}

object ErgoSettingsReaderKeepVersionsProbe {
  def main(args: Array[String]): Unit = {
    val config = ConfigFactory
      .load()
      .withValue("ergo.node.keepVersions", ConfigValueFactory.fromAnyRef(Int.box(0)))
      .withValue(
        "ergo.node.utxo.utxoBootstrap",
        ConfigValueFactory.fromAnyRef(Boolean.box(args.head.toBoolean)))

    ErgoSettingsReader.fromConfig(config)
    println("SETTINGS_ACCEPTED")
  }
}
