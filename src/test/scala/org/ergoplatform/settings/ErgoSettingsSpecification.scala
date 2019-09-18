package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.duration._

class ErgoSettingsSpecification extends ErgoPropertyTest {

  property("should keep data user home  by default") {
    val settings = ErgoSettings.read()
    settings.directory shouldBe System.getProperty("user.dir") + "/.ergo_test/data"
  }

  property("should read default settings") {
    val settings = ErgoSettings.read()
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 1000,
      poPoWBootstrap = false, 10, mining = false, Constants.DefaultComplexityLimit, 1.second, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, keepVersions = 200, mempoolCapacity = 100000, blacklistCapacity = 100000,
      mempoolCleanupDuration = 10.seconds, reBroadcastVolume = 200, minimalFeeAmount = 0)
  }

  property("should read user settings from json file") {
    val settings = ErgoSettings.read(Args(Some("src/test/resources/settings.json"), None))
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 12,
      poPoWBootstrap = false, 10, mining = false, Constants.DefaultComplexityLimit, 1.second, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 10.seconds, reBroadcastVolume = 200, minimalFeeAmount = 0)
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettings.read(Args(Some("src/test/resources/settings.conf"), None))
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 13,
      poPoWBootstrap = false, 10, mining = false, Constants.DefaultComplexityLimit, 1.second, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 10.seconds, reBroadcastVolume = 200, minimalFeeAmount = 0)
  }

}
