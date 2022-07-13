package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.settings.RESTApiSettings

import java.net.InetSocketAddress
import scala.concurrent.duration._

class ErgoSettingsSpecification extends ErgoPropertyTest {

  private val txCostLimit     = initSettings.nodeSettings.maxTransactionCost
  private val txSizeLimit     = initSettings.nodeSettings.maxTransactionSize

  property("should keep data user home  by default") {
    val settings = ErgoSettings.read()
    settings.directory shouldBe System.getProperty("user.dir") + "/.ergo_test/data"
  }

  property("should read default settings") {
    val settings = ErgoSettings.read()
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      1000,
      poPoWBootstrap = false,
      10,
      mining = true,
      txCostLimit,
      txSizeLimit,
      useExternalMiner                          = false,
      internalMinersCount                       = 1,
      internalMinerPollingInterval              = 1.second,
      miningPubKeyHex                           = None,
      offlineGeneration                         = false,
      keepVersions                              = 200,
      acceptableChainUpdateDelay                = 30.minutes,
      mempoolCapacity                           = 100000,
      mempoolCleanupDuration                    = 10.seconds,
      rebroadcastCount                          = 3,
      minimalFeeAmount                          = 0,
      headerChainDiff                           = 100
    )
    settings.cacheSettings shouldBe CacheSettings(
      HistoryCacheSettings(
        12, 100, 1000
      ),
      NetworkCacheSettings(
        invalidModifiersBloomFilterCapacity       = 10000000,
        invalidModifiersBloomFilterExpirationRate = 0.1,
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      ),
      MempoolCacheSettings(
        invalidModifiersBloomFilterCapacity       = 10000000,
        invalidModifiersBloomFilterExpirationRate = 0.1,
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      )
    )
    settings.scorexSettings.restApi shouldBe RESTApiSettings(
      bindAddress = new InetSocketAddress("0.0.0.0", 9052),
      apiKeyHash = None,
      corsAllowedOrigin = Some("*"),
      timeout = 5.seconds
    )
  }

  property("should read user settings from json file") {
    val settings = ErgoSettings.read(Args(Some("src/test/resources/settings.json"), None))
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      12,
      poPoWBootstrap = false,
      10,
      mining = true,
      txCostLimit,
      txSizeLimit,
      useExternalMiner                          = false,
      internalMinersCount                       = 1,
      internalMinerPollingInterval              = 1.second,
      miningPubKeyHex                           = None,
      offlineGeneration                         = false,
      keepVersions                              = 200,
      acceptableChainUpdateDelay                = 30.minutes,
      mempoolCapacity                           = 100000,
      mempoolCleanupDuration                    = 10.seconds,
      rebroadcastCount                          = 3,
      minimalFeeAmount                          = 0,
      headerChainDiff                           = 100
    )
    settings.cacheSettings shouldBe CacheSettings(
      HistoryCacheSettings(
        12, 100, 1000
      ),
      NetworkCacheSettings(
        invalidModifiersBloomFilterCapacity       = 10000000,
        invalidModifiersBloomFilterExpirationRate = 0.1,
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      ),
      MempoolCacheSettings(
        invalidModifiersBloomFilterCapacity       = 10000000,
        invalidModifiersBloomFilterExpirationRate = 0.1,
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      )
    )
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettings.read(Args(Some("src/test/resources/settings.conf"), None))
    settings.nodeSettings shouldBe NodeConfigurationSettings(
      StateType.Utxo,
      verifyTransactions = true,
      13,
      poPoWBootstrap = false,
      10,
      mining = true,
      txCostLimit,
      txSizeLimit,
      useExternalMiner                          = false,
      internalMinersCount                       = 1,
      internalMinerPollingInterval              = 1.second,
      miningPubKeyHex                           = None,
      offlineGeneration                         = false,
      keepVersions                              = 200,
      acceptableChainUpdateDelay                = 30.minutes,
      mempoolCapacity                           = 100000,
      mempoolCleanupDuration                    = 10.seconds,
      rebroadcastCount                          = 3,
      minimalFeeAmount                          = 0,
      headerChainDiff                           = 100
    )
    settings.cacheSettings shouldBe CacheSettings(
      HistoryCacheSettings(
        12, 100, 1000
      ),
      NetworkCacheSettings(
        invalidModifiersBloomFilterCapacity       = 10000000,
        invalidModifiersBloomFilterExpirationRate = 0.1,
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      ),
      MempoolCacheSettings(
        invalidModifiersBloomFilterCapacity       = 10000000,
        invalidModifiersBloomFilterExpirationRate = 0.1,
        invalidModifiersCacheSize                 = 10000,
        invalidModifiersCacheExpiration           = 6.hours,
      )
    )
  }

}
