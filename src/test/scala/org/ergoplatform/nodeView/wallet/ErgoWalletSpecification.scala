package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem}
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.NodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import scorex.crypto.authds.ADKey
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.Random

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

      val prover = new ErgoProvingInterpreter("hello world")
      val pubKey = prover.dlogPubkeys.head

      val address = P2PKAddress(pubKey)
      w.watchFor(address)

      val balance = Random.nextInt(1000) + 1

      val input = Input(ADKey @@ Array.fill(32)(0: Byte), ProverResult(Array.emptyByteArray, ContextExtension(Map())))
      val tx = new ErgoTransaction(IndexedSeq(input), IndexedSeq(new ErgoBoxCandidate(balance, pubKey)))

      w.scanOffchain(tx)

      val bf1 = w.unconfirmedBalances()

      val bs1 = Await.result(bf1, 1.second)

      bs1.balance shouldBe balance
      bs1.assetBalances.isEmpty shouldBe true

      //todo: enhance the test, e.g. add assets
    }
  }

  property("Successfully scans an onchain transaction") {

  }

  property("Successfully doing a rollback"){

  }
}
