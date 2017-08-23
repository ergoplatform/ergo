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


  property("should read default settings") {
    val settings = ErgoSettings.read(None)
    settings.nodeSettings shouldBe NodeConfigurationSettings(ADState = false, verifyTransactions = true, 1000, PoPoWBootstrap = false, 10, 1.minute, 100)
  }


}
