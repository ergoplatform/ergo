package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.SortingOption
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest

import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.concurrent.duration._

class ErgoSettingsSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  val initSettings: ErgoSettings = settings

  private val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
  private val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize

  private def withDirectoryConfig(test: (String, String, String) => Unit): Unit = {
    val directory = Files.createTempDirectory("ergo-settings-")
    val config = Files.createTempFile("ergo-settings-", ".conf")
    val path = directory.toString.replace('\\', '/')
    val secretPath = "ergo.wallet.secretStorage.secretDir"
    val propertyNames = Seq("ergo.directory", secretPath, "config.file")
    val previous = propertyNames.map(name => name -> Option(System.getProperty(name)))
    try {
      propertyNames.foreach(System.clearProperty)
      Files.write(config, s"""ergo.directory = "$path"""".getBytes(StandardCharsets.UTF_8))
      ConfigFactory.invalidateCaches()
      test(config.toString, path, secretPath)
    } finally {
      previous.foreach { case (name, value) =>
        value.fold(System.clearProperty(name))(System.setProperty(name, _))
      }
      ConfigFactory.invalidateCaches()
      Files.deleteIfExists(config)
      Files.deleteIfExists(directory)
    }
  }

  property("secretDir should preserve a system override when the user supplies only a directory") {
    withDirectoryConfig { (config, directory, secretPath) =>
      val explicit = directory + "/explicit-keystore"
      System.setProperty(secretPath, explicit)
      ConfigFactory.invalidateCaches()
      ErgoSettingsReader.read(Args(Some(config))).walletSettings.secretStorage.secretDir shouldBe explicit
    }
  }

  property("secretDir should derive its default from the effective system directory") {
    withDirectoryConfig { (config, directory, _) =>
      val effective = directory + "/effective"
      System.setProperty("ergo.directory", effective)
      ConfigFactory.invalidateCaches()
      val settings = ErgoSettingsReader.read(Args(Some(config)))
      settings.directory shouldBe effective
      settings.walletSettings.secretStorage.secretDir shouldBe effective + "/wallet/keystore"
    }
  }

  property("secretDir should derive its default from the user directory") {
    withDirectoryConfig { (config, directory, _) =>
      ErgoSettingsReader.read(Args(Some(config))).walletSettings.secretStorage.secretDir shouldBe
        directory + "/wallet/keystore"
    }
  }

  property("secretDir should preserve an explicit user value above the application default") {
    withDirectoryConfig { (config, directory, secretPath) =>
      Files.write(java.nio.file.Paths.get(config),
        s"""ergo.directory = "$directory"
           |$secretPath = "$directory"""".stripMargin.getBytes(StandardCharsets.UTF_8))
      ErgoSettingsReader.read(Args(Some(config))).walletSettings.secretStorage.secretDir shouldBe directory
    }
  }

  property("secretDir should preserve system priority over an explicit user value") {
    withDirectoryConfig { (config, directory, secretPath) =>
      Files.write(java.nio.file.Paths.get(config),
        s"""ergo.directory = "$directory"
           |$secretPath = "$directory"""".stripMargin.getBytes(StandardCharsets.UTF_8))
      val explicit = directory + "/explicit-keystore"
      System.setProperty(secretPath, explicit)
      ConfigFactory.invalidateCaches()
      ErgoSettingsReader.read(Args(Some(config))).walletSettings.secretStorage.secretDir shouldBe explicit
    }
  }

  property("secretDir should preserve an explicit application value when the user supplies only a directory") {
    withDirectoryConfig { (config, directory, secretPath) =>
      val application = Files.createTempFile("ergo-application-", ".conf")
      try {
        val explicit = directory + "/application-keystore"
        Files.write(application,
          s"""include classpath("application.conf")
             |$secretPath = "$explicit"""".stripMargin.getBytes(StandardCharsets.UTF_8))
        System.setProperty("config.file", application.toString)
        ConfigFactory.invalidateCaches()
        ErgoSettingsReader.read(Args(Some(config))).walletSettings.secretStorage.secretDir shouldBe explicit
      } finally {
        Files.deleteIfExists(application)
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

}
