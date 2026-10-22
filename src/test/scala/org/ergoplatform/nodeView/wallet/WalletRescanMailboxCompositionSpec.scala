package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{WalletDigest, WalletRegistry}
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.File
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Success, Try}

class WalletRescanMailboxCompositionSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private case class ScanObservation(height: Int, beforeHeight: Int, afterHeight: Int)
  private case class RunResult(status: WalletStatus,
                               committedTip: ModifierId,
                               digest: WalletDigest,
                               scans: Vector[ScanObservation])

  property("a queued catch-up cannot displace or double-count an explicit rescan") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val first = makeGenesisBlock(address.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(address, initialBalance / 2, Array.empty, Map.empty)
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(second) shouldBe 'success
      val fullState = getCurrentState
      val history = getHistory
      history.bestFullBlockAt(first.height).map(_.id) shouldBe Some(first.id)
      history.bestFullBlockAt(second.height).map(_.id) shouldBe Some(second.id)

      def run(name: String,
              fromHeight: Int,
              queueBefore: Boolean,
              queueAfter: Boolean): RunResult = {
        val actorSettings = w.settings.copy(
          directory = new File(w.nodeViewDir, name).getAbsolutePath,
          nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
        )
        actorSettings.nodeSettings.isFullBlocksPruned shouldBe false
        val readEntered = new CountDownLatch(1)
        val releaseRead = new CountDownLatch(1)
        val tipScanEntered = new CountDownLatch(1)
        val releaseTipScan = new CountDownLatch(1)
        val holdFirstTipScan = new AtomicBoolean(true)
        val observations = new ConcurrentLinkedQueue[ScanObservation]()
        val service = new ErgoWalletServiceImpl(actorSettings) {
          override def readWallet(state: ErgoWalletState,
                                  testMnemonic: Option[SecretString],
                                  testKeysQty: Option[Int],
                                  secretStorageSettings: SecretStorageSettings): ErgoWalletState = {
            readEntered.countDown()
            if (!releaseRead.await(10, TimeUnit.SECONDS)) {
              throw new IllegalStateException("timed out waiting to release wallet loading")
            }
            super.readWallet(state, testMnemonic, testKeysQty, secretStorageSettings)
          }

          override def scanBlockUpdate(state: ErgoWalletState,
                                       block: ErgoFullBlock,
                                       dustLimit: Option[Long]): Try[ErgoWalletState] = {
            val before = state.registry.fetchDigest().height
            val result = super.scanBlockUpdate(state, block, dustLimit)
            val after = state.registry.fetchDigest().height
            observations.add(ScanObservation(block.height, before, after))
            if (block.id == second.id && holdFirstTipScan.compareAndSet(true, false)) {
              tipScanEntered.countDown()
              if (!releaseTipScan.await(10, TimeUnit.SECONDS)) {
                throw new IllegalStateException("timed out waiting to release the tip scan")
              }
            }
            result
          }
        }
        val walletSettings = actorSettings.walletSettings
        val selector = new ReplaceCompactCollectBoxSelector(
          walletSettings.maxInputs, walletSettings.optimalInputs, None
        )
        val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, service, selector, history
        )))
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)

        val status = try {
          try {
            readEntered.await(10, TimeUnit.SECONDS) shouldBe true
            probe.send(actor, ChangedState(fullState))
            if (queueBefore) probe.send(actor, ScanOnChain(second))
            probe.send(actor, RescanWallet(fromHeight))
            if (queueAfter) probe.send(actor, ScanOnChain(second))
          } finally releaseRead.countDown()

          probe.expectMsg(10.seconds, Success(()))
          withClue(s"observed scans: ${observations.iterator().asScala.toVector}") {
            tipScanEntered.await(15, TimeUnit.SECONDS) shouldBe true
          }
          // This status request was queued while the tip scan was blocked, so it
          // precedes the actor's self-sent completion message.
          probe.send(actor, GetWalletStatus)
          releaseTipScan.countDown()
          val beforeCompletion = probe.expectMsgType[WalletStatus](10.seconds)
          beforeCompletion.rescanState shouldBe WalletRescanState.InProgress
          probe.send(actor, GetWalletStatus)
          val completed = probe.expectMsgType[WalletStatus](10.seconds)
          completed.rescanState shouldBe WalletRescanState.Inactive
          completed.height shouldBe second.height
          completed.error shouldBe None
          completed
        } finally {
          releaseRead.countDown()
          releaseTipScan.countDown()
          probe.send(actor, CloseWallet)
          probe.expectTerminated(actor, 5.seconds)
        }

        val reopened = WalletRegistry(actorSettings).get
        val (tip, digest) = try reopened.committedVersionAndDigest.get
        finally reopened.close()
        RunResult(status, tip, digest, observations.iterator().asScala.toVector)
      }

      val control = run("rescan-mailbox-control", 0, queueBefore = false, queueAfter = false)
      control.scans.map(_.height) shouldBe Vector(1, 2)
      control.scans.map(_.beforeHeight) shouldBe Vector(0, 1)
      control.scans.map(_.afterHeight) shouldBe Vector(1, 2)
      control.committedTip shouldBe second.id
      control.digest.height shouldBe second.height

      val after = run("rescan-mailbox-after", 0, queueBefore = false, queueAfter = true)
      after.scans shouldBe control.scans
      after.committedTip shouldBe second.id
      after.digest shouldBe control.digest

      val before = run("rescan-mailbox-before", 1, queueBefore = true, queueAfter = false)
      before.scans shouldBe control.scans
      before.committedTip shouldBe second.id
      before.digest shouldBe control.digest
    }
  }
}
