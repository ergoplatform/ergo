package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scala.concurrent.duration._
import scala.concurrent.Await

class ErgoWalletSpecification extends ErgoPropertyTest {

  type H = ErgoHistory
  type S = ErgoState[_]
  type D = DigestState
  type U = UtxoState
  type W = ErgoWallet
  type P = ErgoMemPool


  property("Successfully scans an offchain transaction") {

    implicit val actorSystem = ActorSystem()
    val ergoSettings = ErgoSettings.read(None)
    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)

    nodeViewHolderRef ! GetDataFromCurrentView[H, S, W, P, Any] { v =>
      val w = v.vault
      val bf0 = w.unconfirmedBalances()

      val bs0 = Await.result(bf0, 1.second)

      bs0.balance shouldBe 0
      bs0.assetBalances.isEmpty shouldBe true

      //todo: check before and after resolving
    }
  }

  property("Successfully doing a rollback"){

  }
}
