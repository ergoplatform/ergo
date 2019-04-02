package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.duration._

class ErgoSettingsSpecification extends ErgoPropertyTest {

  property("should keep data user home  by default") {
    val settings = ErgoSettings.read(None)
    settings.directory shouldBe System.getProperty("user.dir") + "/ergo/data"
  }

  property("should read default settings") {
    val settings = ErgoSettings.read(None)
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 1000,
      poPoWBootstrap = false, 10, mining = false, 1.second, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, keepVersions = 200, mempoolCapacity = 100000, blacklistCapacity = 100000,
      mempoolCleanupDuration = 10.seconds)
  }

  property("should read user settings from json file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.json"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 12,
      poPoWBootstrap = false, 10, mining = false, 1.second, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 10.seconds)
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.conf"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 13,
      poPoWBootstrap = false, 10, mining = false, 1.second, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 10.seconds)
  }

}
