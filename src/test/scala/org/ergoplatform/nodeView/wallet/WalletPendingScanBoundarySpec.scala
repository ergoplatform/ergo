package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.ErgoBox
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedHistory
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{BoxUtils, ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.validTransactionFromBoxes
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import org.ergoplatform.wallet.Constants.PaymentsScanId
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.boxes.ChainStatus
import org.scalatest.concurrent.Eventually

import java.io.File
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Success, Try}

class WalletPendingScanBoundarySpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private case class ScanObservation(height: Int, beforeBalance: Long, afterBalance: Long,
                                     succeeded: Boolean)

  property("rescan discards a missing-body retry from the previous registry") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val first = makeGenesisBlock(address.pubkey)
      applyBlock(first) shouldBe 'success
      val walletBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(address, walletBalance / 2, Array.empty, Map.empty)
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(spendingTx))
      val history = getHistory

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "pending-scan-rescan").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val rescanEntered = new CountDownLatch(1)
      val releaseRescan = new CountDownLatch(1)
      val firstRescanEntered = new CountDownLatch(1)
      val releaseFirstRescan = new CountDownLatch(1)
      val secondScanEntered = new CountDownLatch(1)
      val releaseSecondScan = new CountDownLatch(1)
      val holdFirstSecondScan = new AtomicBoolean(true)
      val firstScanCount = new AtomicInteger(0)
      val observations = new ConcurrentLinkedQueue[ScanObservation]()
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def recreateRegistry(state: ErgoWalletState,
                                      settings: ErgoSettings): Try[ErgoWalletState] = {
          val recreated = super.recreateRegistry(state, settings)
          rescanEntered.countDown()
          if (!releaseRescan.await(10, TimeUnit.SECONDS)) {
            throw new IllegalStateException("registry recreation gate timed out")
          }
          recreated
        }

        override def scanBlockUpdate(state: ErgoWalletState, block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val before = state.registry.fetchDigest().walletBalance
          val result = super.scanBlockUpdate(state, block, dustLimit)
          val after = state.registry.fetchDigest().walletBalance
          observations.add(ScanObservation(block.height, before, after, result.isSuccess))
          if (block.height == first.height && firstScanCount.incrementAndGet() == 2) {
            firstRescanEntered.countDown()
            if (!releaseFirstRescan.await(10, TimeUnit.SECONDS)) {
              throw new IllegalStateException("first rescan gate timed out")
            }
          }
          if (block.height == second.height &&
              holdFirstSecondScan.compareAndSet(true, false)) {
            secondScanEntered.countDown()
            if (!releaseSecondScan.await(10, TimeUnit.SECONDS)) {
              throw new IllegalStateException("second scan gate timed out")
            }
          }
          result
        }
      }
      val walletSettings = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(
        walletSettings.maxInputs, walletSettings.optimalInputs, None)
      val actorProps = Props(classOf[ErgoWalletActor], actorSettings, parameters,
        service, selector, history)
      val actor = w.actorSystem.actorOf(actorProps)
      val probe = TestProbe()(w.actorSystem)

      try {
        probe.awaitAssert({
          probe.send(actor, GetWalletStatus)
          probe.expectMsgType[WalletStatus].height shouldBe first.height
        }, 10.seconds, 100.millis)

        probe.send(actor, ScanInThePast(second.height, rescan = false))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe first.height

        probe.send(actor, RescanWallet(first.height))
        rescanEntered.await(10, TimeUnit.SECONDS) shouldBe true
        // This event is ahead of the rescan's self-sent first block in the mailbox.
        probe.send(actor, ChangedHistory(history))
        releaseRescan.countDown()
        probe.expectMsg(Success(()))

        firstRescanEntered.await(10, TimeUnit.SECONDS) shouldBe true
        try {
          applyBlock(second) shouldBe 'success
          eventually(timeout(10.seconds), interval(100.millis)) {
            history.fullBlockHeight should be >= second.height
            history.isSelectedFullBlockId(second.id) shouldBe true
            history.bestFullBlockAt(second.height).map(_.id) shouldBe Some(second.id)
          }
          probe.send(actor, ChangedState(getCurrentState))
        } finally {
          releaseFirstRescan.countDown()
        }

        secondScanEntered.await(10, TimeUnit.SECONDS) shouldBe true
        // The status barrier follows any h2 rescan step already in the mailbox.
        probe.send(actor, GetWalletStatus)
        releaseSecondScan.countDown()
        val status = probe.expectMsgType[WalletStatus]
        status.height shouldBe second.height
        status.error shouldBe None
        val expectedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
          val digest = getConfirmedBalances
          digest.height shouldBe second.height
          digest.walletBalance
        }
        val scans = observations.iterator().asScala.toVector
        withClue(s"observed wallet scans: $scans") {
          scans.map(_.height) shouldBe Vector(first.height, first.height, second.height)
          scans.forall(_.succeeded) shouldBe true
          scans.last.afterBalance shouldBe expectedBalance
        }
      } finally {
        releaseRescan.countDown()
        releaseFirstRescan.countDown()
        releaseSecondScan.countDown()
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("pending ordinary scan resumes after wallet rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val first = makeGenesisBlock(address.pubkey)
      applyBlock(first) shouldBe 'success
      val firstBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val digest = getConfirmedBalances
        digest.height shouldBe first.height
        digest.walletBalance
      }
      val secondPayment = PaymentRequest(address, firstBalance / 2, Array.empty, Map.empty)
      val secondTx = await(wallet.generateTransaction(Seq(secondPayment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val secondBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val digest = getConfirmedBalances
        digest.height shouldBe second.height
        digest.walletBalance
      }
      val thirdPayment = PaymentRequest(address, secondBalance / 4, Array.empty, Map.empty)
      val thirdTx = await(wallet.generateTransaction(Seq(thirdPayment))).get
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      val history = getHistory
      history.getFullBlock(third.header) shouldBe None

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "pending-scan-rollback").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = 3)
      )
      val service = new ErgoWalletServiceImpl(actorSettings)
      val walletSettings = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(
        walletSettings.maxInputs, walletSettings.optimalInputs, None)
      val actorProps = Props(classOf[ErgoWalletActor], actorSettings, parameters,
        service, selector, history)
      val actor = w.actorSystem.actorOf(actorProps)
      val probe = TestProbe()(w.actorSystem)

      try {
        probe.awaitAssert({
          probe.send(actor, GetWalletStatus)
          val status = probe.expectMsgType[WalletStatus]
          status.height shouldBe second.height
          status.error shouldBe None
        }, 10.seconds, 100.millis)
        // The Holder's wallet remains subscribed; this actor receives only the chosen history signal.
        w.actorSystem.eventStream.unsubscribe(actor, classOf[ChangedHistory])
        w.actorSystem.eventStream.unsubscribe(actor, classOf[ChangedState])

        probe.send(actor, ScanInThePast(third.height, rescan = false))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe second.height
        probe.send(actor, Rollback(idToVersion(first.id)))
        probe.send(actor, GetWalletStatus)
        val rolledBack = probe.expectMsgType[WalletStatus]
        rolledBack.height shouldBe first.height
        rolledBack.error shouldBe None

        applyBlock(third) shouldBe 'success
        eventually(timeout(10.seconds), interval(100.millis)) {
          history.fullBlockHeight shouldBe third.height
          history.isSelectedFullBlockId(third.id) shouldBe true
          history.getFullBlock(third.header).isDefined shouldBe true
        }
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe first.height

        probe.send(actor, ChangedHistory(history))
        withClue("pending rollback retry did not reach h3: ") {
          probe.awaitAssert({
            probe.send(actor, GetWalletStatus)
            val status = probe.expectMsgType[WalletStatus]
            status.height shouldBe third.height
            status.error shouldBe None
          }, 10.seconds, 100.millis)
        }
        val expectedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
          val digest = getConfirmedBalances
          digest.height shouldBe third.height
          digest.walletBalance
        }
        probe.send(actor, ReadBalances(ChainStatus.OnChain))
        val recoveredDigest = probe.expectMsgType[WalletDigest]
        recoveredDigest.height shouldBe third.height
        recoveredDigest.walletBalance shouldBe expectedBalance
        probe.send(actor, GetTransaction(secondTx.id))
        probe.expectMsgType[Option[AugWalletTransaction]].isDefined shouldBe true
        probe.send(actor, GetTransaction(thirdTx.id))
        probe.expectMsgType[Option[AugWalletTransaction]].isDefined shouldBe true
      } finally {
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("pending ordinary scan follows selected fork after real pruning and rollback") {
    val ordinarySettings = org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
    val prunedSettings = ordinarySettings.copy(
      nodeSettings = ordinarySettings.nodeSettings.copy(blocksToKeep = 3))

    new WalletFixture(prunedSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val address = getPublicKeys.head
      val (producerState, producerBoxes) = createUtxoState(w.settings)
      val genesisState = WrappedUtxoState(producerState, producerBoxes, w.settings)
      val first = validFullBlock(None, genesisState)
      val afterFirst = genesisState.applyModifier(first)(_ => ()).get
      applyBlock(first) shouldBe 'success
      val history = getHistory
      eventually(timeout(10.seconds), interval(100.millis)) {
        history.fullBlockHeight shouldBe first.height
        getCurrentState.stateContext.currentHeight shouldBe first.height
      }

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "pending-scan-pruned-fork").getAbsolutePath)
      val service = new ErgoWalletServiceImpl(actorSettings)
      val walletSettings = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(
        walletSettings.maxInputs, walletSettings.optimalInputs, None)
      val actorProps = Props(classOf[ErgoWalletActor], actorSettings, parameters,
        service, selector, history)
      val actor = w.actorSystem.actorOf(actorProps)
      val probe = TestProbe()(w.actorSystem)

      try {
        probe.awaitAssert({
          probe.send(actor, GetWalletStatus)
          val status = probe.expectMsgType[WalletStatus]
          status.height shouldBe first.height
          status.initialized shouldBe true
          status.error shouldBe None
        }, 10.seconds, 100.millis)
        // Subscribe only through A1; all later wallet signals come from this probe in order.
        w.actorSystem.eventStream.unsubscribe(actor, classOf[ChangedHistory])
        w.actorSystem.eventStream.unsubscribe(actor, classOf[ChangedState])
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe first.height

        val minTxInputValue = BoxUtils.sufficientAmount(
          org.ergoplatform.utils.ErgoNodeTestConstants.extendedParameters)
        def spendable(state: WrappedUtxoState, minValue: Long): ErgoBox =
          state.takeBoxes(state.size)
            .filter(box => box.ergoTree == TrueTree && box.value >= minValue)
            .maxBy(_.value)

        val a2Tx = validTransactionFromBoxes(
          IndexedSeq(spendable(afterFirst, 2 * minTxInputValue)),
          issueNew = false, outputsProposition = address.script)
        val a2 = validFullBlock(Some(first), afterFirst, Seq(a2Tx))
        val afterA2 = afterFirst.applyModifier(a2)(_ => ()).get
        val a3Tx = validTransactionFromBoxes(
          IndexedSeq(spendable(afterA2, 2 * minTxInputValue)),
          issueNew = false, outputsProposition = TrueTree)
        val a3 = validFullBlock(Some(a2), afterA2, Seq(a3Tx))
        val afterA3 = afterA2.applyModifier(a3)(_ => ()).get
        val a4Tx = validTransactionFromBoxes(
          IndexedSeq(spendable(afterA3, minTxInputValue)),
          issueNew = false, outputsProposition = address.script)
        val a4 = validFullBlock(Some(a3), afterA3, Seq(a4Tx))
        val afterA4 = afterA3.applyModifier(a4)(_ => ()).get
        val a4Credit = a4Tx.outputs.map(_.value).sum
        a4Credit should be > 0L

        def applyAndScan(block: ErgoFullBlock): Unit = {
          applyBlock(block) shouldBe 'success
          eventually(timeout(10.seconds), interval(100.millis)) {
            history.fullBlockHeight shouldBe block.height
            history.isSelectedFullBlockId(block.id) shouldBe true
            getCurrentState.stateContext.currentHeight shouldBe block.height
          }
          probe.send(actor, ChangedState(getCurrentState))
          probe.send(actor, ScanOnChain(block))
          probe.awaitAssert({
            probe.send(actor, GetWalletStatus)
            val status = probe.expectMsgType[WalletStatus]
            status.height shouldBe block.height
            status.error shouldBe None
          }, 10.seconds, 100.millis)
        }

        applyAndScan(a2)
        probe.send(actor, ReadBalances(ChainStatus.OnChain))
        val baseDigest = probe.expectMsgType[WalletDigest]
        baseDigest.height shouldBe a2.height
        applyAndScan(a3)
        eventually(timeout(10.seconds), interval(100.millis)) {
          history.walletScannedHeight.getOrElse(0) should be >= a3.height
        }
        applyAndScan(a4)
        eventually(timeout(10.seconds), interval(100.millis)) {
          history.readPrunedHeight() should be > first.height
          history.getFullBlock(first.header) shouldBe None
          history.getFullBlock(a2.header).isDefined shouldBe true
        }
        probe.send(actor, ReadBalances(ChainStatus.OnChain))
        val aDigest = probe.expectMsgType[WalletDigest]
        aDigest.height shouldBe a4.height
        aDigest.walletBalance shouldBe (baseDigest.walletBalance + a4Credit)
        probe.send(actor, GetTransaction(a4Tx.id))
        probe.expectMsgType[Option[AugWalletTransaction]]
          .exists(_.wtx.scanIds.contains(PaymentsScanId)) shouldBe true

        // The ordinary h5 request is pending while History still ends at A4.
        probe.send(actor, ScanInThePast(5, rescan = false))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe a4.height

        val bForkState = afterA4.rollbackTo(afterA2.version).get
        val b3Tx = validTransactionFromBoxes(
          IndexedSeq(spendable(bForkState, 2 * minTxInputValue)),
          issueNew = true, outputsProposition = TrueTree)
        b3Tx.outputs.size should be >= 2
        val b3 = validFullBlock(Some(a2), bForkState, Seq(b3Tx))
        b3.id should not be a3.id
        val afterB3 = bForkState.applyModifier(b3)(_ => ()).get
        val b4Tx = validTransactionFromBoxes(
          IndexedSeq(spendable(afterB3, minTxInputValue)),
          issueNew = false, outputsProposition = address.script)
        val b4 = validFullBlock(Some(b3), afterB3, Seq(b4Tx))
        b4Tx.id should not be a4Tx.id
        val afterB4 = afterB3.applyModifier(b4)(_ => ()).get
        // B3 makes at least two spendable outputs; B4 consumes only one of them.
        val b5Tx = validTransactionFromBoxes(
          IndexedSeq(spendable(afterB4, minTxInputValue)),
          issueNew = false, outputsProposition = TrueTree)
        val b5 = validFullBlock(Some(b4), afterB4, Seq(b5Tx))
        val b4Credit = b4Tx.outputs.map(_.value).sum
        b4Credit should be > 0L

        Seq(b3, b4, b5).foreach(block => applyHeader(block.header) shouldBe 'success)
        Seq(b3, b4, b5).foreach(block => applyPayload(block) shouldBe 'success)
        eventually(timeout(10.seconds), interval(100.millis)) {
          history.fullBlockHeight shouldBe b5.height
          history.isSelectedFullBlockId(b5.id) shouldBe true
          history.bestFullBlockAt(b4.height).map(_.id) shouldBe Some(b4.id)
          getCurrentState.stateContext.currentHeight shouldBe b5.height
          history.getFullBlock(first.header) shouldBe None
        }

        probe.send(actor, Rollback(idToVersion(a2.id)))
        probe.send(actor, GetWalletStatus)
        val rolledBack = probe.expectMsgType[WalletStatus]
        rolledBack.height shouldBe a2.height
        rolledBack.error shouldBe None
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe a2.height
        // Without the selected-history signal, the pending retry cannot advance this actor.
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus].height shouldBe a2.height

        probe.send(actor, ChangedHistory(history))
        withClue("pruned fork pending retry did not reach B5: ") {
          probe.awaitAssert({
            probe.send(actor, GetWalletStatus)
            val status = probe.expectMsgType[WalletStatus]
            status.height shouldBe b5.height
            status.error shouldBe None
          }, 10.seconds, 100.millis)
        }
        probe.send(actor, GetTransaction(a4Tx.id))
        probe.expectMsgType[Option[AugWalletTransaction]] shouldBe None
        probe.send(actor, GetTransaction(b4Tx.id))
        probe.expectMsgType[Option[AugWalletTransaction]]
          .exists(_.wtx.scanIds.contains(PaymentsScanId)) shouldBe true
        probe.send(actor, ReadBalances(ChainStatus.OnChain))
        val bDigest = probe.expectMsgType[WalletDigest]
        bDigest.height shouldBe b5.height
        bDigest.walletBalance shouldBe (baseDigest.walletBalance + b4Credit)
      } finally {
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }
}
