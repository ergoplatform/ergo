package org.ergoplatform.utils.fixtures

import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.{GetWalletStatus, WalletStatus}
import org.ergoplatform.settings.{ErgoSettings, Parameters}

import scala.concurrent.Await
import scala.concurrent.duration._

class WalletFixture(
  settings: ErgoSettings,
  params: Parameters,
  getWallet: WalletFixture => ErgoWallet
) extends NodeViewFixture(settings, params) {
  val wallet: ErgoWallet = getWallet(this)

  private def awaitStartupAlignment(): Unit = {
    implicit val timeout: Timeout = Timeout(5.seconds)
    val deadline = 10.seconds.fromNow
    def currentStatus: WalletStatus =
      Await.result(wallet.walletActor ? GetWalletStatus, timeout.duration).asInstanceOf[WalletStatus]

    var status = currentStatus
    while (status.error.exists(_.contains("startup canonical alignment")) && deadline.hasTimeLeft()) {
      Thread.sleep(50)
      status = currentStatus
    }
    require(
      !status.error.exists(_.contains("startup canonical alignment")),
      "Wallet fixture did not complete startup canonical alignment")
  }

  awaitStartupAlignment()
}
