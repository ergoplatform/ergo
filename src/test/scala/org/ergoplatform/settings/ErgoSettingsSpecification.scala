package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoPropertyTest

import scala.concurrent.duration._

class ErgoSettingsSpecification extends ErgoPropertyTest {

  private val niPoPowSettings = NiPoPowSettings(enabled = false, 30, 30, 30, 0.45f)

  property("should keep data user home  by default") {
    val settings = ErgoSettings.read(None)
    settings.directory shouldBe System.getProperty("user.dir") + "/ergo/data"
  }

  property("should read default settings") {
    val settings = ErgoSettings.read(None)
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 10,
      mining = false, 1.second, offlineGeneration = false, 200, niPoPowSettings)
  }

  property("should read user settings from json file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.json"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 10,
      mining = false, 1.second, offlineGeneration = false, 200, niPoPowSettings)
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.conf"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(StateType.Utxo, verifyTransactions = true, 10,
      mining = false, 1.second, offlineGeneration = false, 200, niPoPowSettings)
  }

}
