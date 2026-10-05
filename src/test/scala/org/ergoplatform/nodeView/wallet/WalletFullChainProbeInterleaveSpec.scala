package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedHistory
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletRegistry
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.File
import scala.concurrent.duration._

class WalletFullChainProbeInterleaveSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("history notification preserves a pending probe cursor while the full tip is fixed") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "full-chain-probe-interleave").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val actorProbe = TestProbe()(w.actorSystem)
      actorProbe.watch(actor)
      try {
        actorProbe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally {
        actorProbe.send(actor, CloseWallet)
        actorProbe.expectTerminated(actor, 5.seconds)
      }

      val registry = WalletRegistry(actorSettings).get
      try {
        val (tip, digest) = registry.committedVersionAndDigest.get
        tip shouldBe first.id
        digest.height shouldBe first.height
      } finally registry.close()

      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        initialBalance / 2,
        Array.empty,
        Map.empty
      )
      val secondTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val history = getHistory
      history.bestFullBlockIdOpt shouldBe Some(second.id)
      val expectedCursor = history.selectedFullChainProbe(
        first.id, first.height, None, maxHeaders = 1
      ) match {
        case FullChainPending(cursor) => cursor
        case result => fail(s"Expected a pending one-header history probe, got $result")
      }

      val observations = TestProbe()(w.actorSystem)
      val reopened = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        private var probeCount = 0

        override protected def probeSelectedFullChain(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe = {
          probeCount += 1
          observations.ref ! cursor
          if (probeCount == 1) {
            // This reaches the mailbox before the actor enqueues its Pending continuation.
            self ! ChangedHistory(history)
            FullChainPending(expectedCursor)
          } else {
            // Finish the real history probe so the assertion can also check wallet loading.
            history.selectedFullChainProbe(targetId, targetHeight,
              Some(expectedCursor), maxHeaders = 1)
          }
        }
      }))
      val reopenedProbe = TestProbe()(w.actorSystem)
      reopenedProbe.watch(reopened)
      try {
        observations.expectMsg(None)
        val resumedCursor = observations.expectMsgType[Option[FullChainCursor]](5.seconds)
        reopenedProbe.send(reopened, GetWalletStatus)
        val status = reopenedProbe.expectMsgType[WalletStatus](5.seconds)
        status.height shouldBe first.height
        status.error shouldBe None
        resumedCursor shouldBe Some(expectedCursor)
      } finally {
        reopenedProbe.send(reopened, CloseWallet)
        reopenedProbe.expectTerminated(reopened, 5.seconds)
      }
    }
  }

  property("unknown full-chain probes keep one retry schedule across history notifications") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "full-chain-unknown-retry").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val actorProbe = TestProbe()(w.actorSystem)
      actorProbe.watch(actor)
      try {
        actorProbe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally {
        actorProbe.send(actor, CloseWallet)
        actorProbe.expectTerminated(actor, 5.seconds)
      }

      val emptyHistorySettings = actorSettings.copy(
        directory = new File(w.nodeViewDir, "history-without-full-tip").getAbsolutePath
      )
      val emptyHistory = ErgoHistory.readOrGenerate(emptyHistorySettings)(null)
      try {
        emptyHistory.bestFullBlockIdOpt shouldBe None
        emptyHistory.selectedFullChainProbe(first.id, first.height) shouldBe FullChainUnknown
        val observations = TestProbe()(w.actorSystem)
        val waiting = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, emptyHistory
        ) {
          private var probeCount = 0

          override protected def probeSelectedFullChain(
            targetId: ModifierId,
            targetHeight: Int,
            cursor: Option[FullChainCursor]
          ): FullChainProbe = {
            probeCount += 1
            observations.ref ! probeCount
            if (probeCount == 1) {
              // All five notifications enter the mailbox before the first Unknown timer is set.
              (1 to 5).foreach(_ => self ! ChangedHistory(emptyHistory))
            }
            emptyHistory.selectedFullChainProbe(targetId, targetHeight, cursor)
          }
        }))
        val waitingProbe = TestProbe()(w.actorSystem)
        waitingProbe.watch(waiting)
        try {
          observations.expectMsg(1)
          // Akka may scale this window, so bound retries by elapsed wall time.
          val startedAt = System.nanoTime()
          val laterProbes = observations.receiveWhile(
            max = 5300.millis, idle = 5300.millis
          ) { case n: java.lang.Integer => n.intValue() }
          val elapsed = System.nanoTime() - startedAt
          val periodicAllowance = math.ceil(elapsed.toDouble / 2.seconds.toNanos).toInt
          laterProbes.size should be >= 2
          laterProbes.size should be <= (5 + periodicAllowance + 2)
          waitingProbe.send(waiting, GetWalletStatus)
          val status = waitingProbe.expectMsgType[WalletStatus](5.seconds)
          status.height shouldBe first.height
          status.error should not be None
        } finally {
          waitingProbe.send(waiting, CloseWallet)
          waitingProbe.expectTerminated(waiting, 5.seconds)
        }
      } finally emptyHistory.closeStorage()
    }
  }
}
