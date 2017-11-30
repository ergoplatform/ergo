package org.ergoplatform.settings

import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._

class ErgoSettingsSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators with scorex.testkit.SerializationTests {

  property("should keep data user home  by default") {
    val settings = ErgoSettings.read(None)
    settings.directory shouldBe System.getProperty("user.dir") + "/ergo/data"
  }

  property("should read default settings") {
    val settings = ErgoSettings.read(None)
    settings.nodeSettings shouldBe NodeConfigurationSettings(ADState = false, verifyTransactions = true, 1000,
      PoPoWBootstrap = false, 10, false, 1.second, false)
  }

  property("should read user settings from json file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.json"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(ADState = false, verifyTransactions = true, 12,
      PoPoWBootstrap = false, 10, false, 1.second, false)
  }

  property("should read user settings from HOCON file") {
    val settings = ErgoSettings.read(Some("src/test/resources/settings.conf"))
    settings.nodeSettings shouldBe NodeConfigurationSettings(ADState = false, verifyTransactions = true, 13,
      PoPoWBootstrap = false, 10, false, 1.second, false)
  }


}
