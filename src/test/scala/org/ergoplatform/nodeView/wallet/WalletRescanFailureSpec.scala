package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletRegistry
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class WalletRescanFailureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("rescan failure cannot be hidden by a later durable wallet checkpoint") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val pubKey = address.pubkey
      val first = makeGenesisBlock(pubKey)
      applyBlock(first) shouldBe 'success
      val walletBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(address, walletBalance / 2, Array.empty, Map.empty)
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(second) shouldBe 'success

      def runRescan(name: String, failFirst: Boolean): (WalletStatus, Int, Boolean) = {
        val actorSettings = w.settings.copy(
          directory = new File(w.nodeViewDir, name).getAbsolutePath
        )
        val injectedFailure = new AtomicBoolean(failFirst)
        val service = new ErgoWalletServiceImpl(actorSettings) {
          override def scanBlockUpdate(
            state: ErgoWalletState,
            block: ErgoFullBlock,
            dustLimit: Option[Long]
          ): Try[ErgoWalletState] = {
            if (block.id == first.id && injectedFailure.compareAndSet(true, false)) {
              Failure(new IllegalStateException("one-shot scan failure"))
            } else {
              super.scanBlockUpdate(state, block, dustLimit)
            }
          }
        }
        val walletSettings = actorSettings.walletSettings
        val selector = new ReplaceCompactCollectBoxSelector(
          walletSettings.maxInputs,
          walletSettings.optimalInputs,
          None
        )
        val actor = w.actorSystem.actorOf(
          Props(classOf[ErgoWalletActor], actorSettings, parameters, service, selector, getHistory)
        )
        val probe = TestProbe()(w.actorSystem)
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, RescanWallet(1))
        probe.expectMsg(scala.util.Success(()))

        eventually(timeout(10.seconds), interval(100.millis)) {
          probe.send(actor, GetWalletStatus)
          val current = probe.expectMsgType[WalletStatus]
          if (failFirst) {
            current.error.value should include("one-shot scan failure")
          } else {
            current.height shouldBe second.height
          }
          current
        }

        // The actor queued a subsequent scan before replying if it continued past the error.
        probe.send(actor, GetWalletStatus)
        val settledStatus = probe.expectMsgType[WalletStatus]
        if (failFirst) settledStatus.height shouldBe 0

        val finalStatus = if (failFirst) {
          probe.send(actor, RescanWallet(1))
          probe.expectMsg(scala.util.Success(()))
          eventually(timeout(10.seconds), interval(100.millis)) {
            probe.send(actor, GetWalletStatus)
            val retried = probe.expectMsgType[WalletStatus]
            retried.height shouldBe second.height
            retried.error.value should include("one-shot scan failure")
            retried
          }
        } else settledStatus

        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)

        val registry = WalletRegistry(actorSettings).get
        try {
          val digestHeight = registry.fetchDigest().height
          val sawFirstTransaction = registry.allWalletTxs().exists(_.tx.id == first.transactions.head.id)
          (finalStatus, digestHeight, sawFirstTransaction)
        } finally {
          registry.close()
        }
      }

      val control = runRescan("control", failFirst = false)
      control._1.error shouldBe None
      control._2 shouldBe second.height
      control._3 shouldBe true

      val failed = runRescan("failed", failFirst = true)
      failed._1.error.value should include("one-shot scan failure")
      failed._2 shouldBe second.height
      failed._3 shouldBe true
    }
  }
}
