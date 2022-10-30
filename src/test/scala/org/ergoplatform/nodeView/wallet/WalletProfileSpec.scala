package org.ergoplatform.nodeView.wallet

import org.ergoplatform.utils.ErgoPropertyTest

import scala.util.Try

class WalletProfileSpec extends ErgoPropertyTest {
  property("fromLabel getting profiles properly") {
    WalletProfile.fromLabel("user").outputsFilterSize shouldBe WalletProfile.User.outputsFilterSize
    WalletProfile.fromLabel("user").scriptsFilterSize shouldBe WalletProfile.User.scriptsFilterSize

    WalletProfile.fromLabel("exchange").scriptsFilterSize should not be WalletProfile.User.scriptsFilterSize
    WalletProfile.fromLabel("exchange").outputsFilterSize shouldBe WalletProfile.Exchange.outputsFilterSize
    WalletProfile.fromLabel("exchange").scriptsFilterSize shouldBe WalletProfile.Exchange.scriptsFilterSize

    Try(WalletProfile.fromLabel("appserver")).isFailure shouldBe true
    WalletProfile.fromLabel("appServer").outputsFilterSize shouldBe WalletProfile.AppServer.outputsFilterSize
    WalletProfile.fromLabel("appServer").scriptsFilterSize shouldBe WalletProfile.AppServer.scriptsFilterSize
  }
}
