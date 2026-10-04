package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{WalletDigest, WalletRegistry}
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.scalatest.concurrent.Eventually

import java.io.File
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.{Success, Try}

class WalletRescanMailboxRaceSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private case class ScanObservation(
    blockHeight: Int,
    beforeHeight: Int,
    beforeBalance: Long,
    afterHeight: Int,
    afterBalance: Long,
    succeeded: Boolean
  )

  private case class RunResult(
    status: WalletStatus,
    digest: WalletDigest,
    scans: Vector[ScanObservation]
  )

  property("a rescan ignores catch-ups queued before or after the API-default request") {
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
      applyBlock(second) shouldBe 'success
      first.height shouldBe 1
      second.height shouldBe 2

      val fullState = getCurrentState
      fullState.stateContext.currentHeight shouldBe second.height
      val history = getHistory
      history.bestFullBlockAt(first.height).map(_.id) shouldBe Some(first.id)
      history.bestFullBlockAt(second.height).map(_.id) shouldBe Some(second.id)

      def run(
        name: String,
        fromHeight: Int,
        queueBeforeRescan: Boolean,
        queueAfterRescan: Boolean
      ): RunResult = {
        val actorSettings = w.settings.copy(
          directory = new File(w.nodeViewDir, name).getAbsolutePath,
          nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
        )
        actorSettings.nodeSettings.isFullBlocksPruned shouldBe false
        val readEntered = new CountDownLatch(1)
        val releaseRead = new CountDownLatch(1)
        val secondScanEntered = new CountDownLatch(1)
        val releaseSecondScan = new CountDownLatch(1)
        val holdFirstSecondScan = new AtomicBoolean(true)
        val initialHeight = new AtomicInteger(-1)
        val recreations = new AtomicInteger(0)
        val observations = new ConcurrentLinkedQueue[ScanObservation]()
        val service = new ErgoWalletServiceImpl(actorSettings) {
          override def readWallet(
            state: ErgoWalletState,
            testMnemonic: Option[SecretString],
            testKeysQty: Option[Int],
            secretStorageSettings: SecretStorageSettings
          ): ErgoWalletState = {
            initialHeight.set(state.getWalletHeight)
            readEntered.countDown()
            if (!releaseRead.await(10, TimeUnit.SECONDS)) {
              throw new IllegalStateException("timed out waiting to release wallet loading")
            }
            super.readWallet(state, testMnemonic, testKeysQty, secretStorageSettings)
          }

          override def recreateRegistry(
            state: ErgoWalletState,
            settings: ErgoSettings
          ): Try[ErgoWalletState] = {
            recreations.incrementAndGet()
            super.recreateRegistry(state, settings)
          }

          override def scanBlockUpdate(
            state: ErgoWalletState,
            block: ErgoFullBlock,
            dustLimit: Option[Long]
          ): Try[ErgoWalletState] = {
            val before = state.registry.fetchDigest()
            val result = super.scanBlockUpdate(state, block, dustLimit)
            val after = state.registry.fetchDigest()
            observations.add(ScanObservation(
              block.height,
              before.height,
              before.walletBalance,
              after.height,
              after.walletBalance,
              result.isSuccess
            ))
            if (block.height == second.height && holdFirstSecondScan.compareAndSet(true, false)) {
              secondScanEntered.countDown()
              if (!releaseSecondScan.await(10, TimeUnit.SECONDS)) {
                throw new IllegalStateException("timed out waiting to release the second scan")
              }
            }
            result
          }
        }
        val walletSettings = actorSettings.walletSettings
        val selector = new ReplaceCompactCollectBoxSelector(
          walletSettings.maxInputs,
          walletSettings.optimalInputs,
          None
        )
        val actor = w.actorSystem.actorOf(
          Props(classOf[ErgoWalletActor], actorSettings, parameters, service, selector, history)
        )
        val probe = TestProbe()(w.actorSystem)

        try {
          readEntered.await(10, TimeUnit.SECONDS) shouldBe true
          initialHeight.get() shouldBe 0
          probe.send(actor, ChangedState(fullState))
          if (queueBeforeRescan) probe.send(actor, ScanOnChain(second))
          probe.send(actor, RescanWallet(fromHeight))
          if (queueAfterRescan) probe.send(actor, ScanOnChain(second))
        } finally {
          releaseRead.countDown()
        }

        probe.expectMsg(Success(()))
        try {
          withClue(s"observed scans: ${observations.iterator().asScala.toVector}") {
            secondScanEntered.await(15, TimeUnit.SECONDS) shouldBe true
          }
          // The status request follows the last queued rescan step on both paths.
          probe.send(actor, GetWalletStatus)
        } finally {
          releaseSecondScan.countDown()
        }
        val status = probe.expectMsgType[WalletStatus]
        val scans = observations.iterator().asScala.toVector
        recreations.get() shouldBe 1

        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)

        val reopened = WalletRegistry(actorSettings).get
        val digest = try reopened.fetchDigest() finally reopened.close()
        RunResult(status, digest, scans)
      }

      val defaultControl = run("default-single-pass", 0, false, false)
      defaultControl.scans.map(_.blockHeight) shouldBe Vector(1, 2)
      defaultControl.scans.forall(_.succeeded) shouldBe true
      defaultControl.status.error shouldBe None
      defaultControl.digest.height shouldBe second.height
      defaultControl.digest.walletBalance should be > 0L

      val defaultRaced = run("default-live-after-rescan", 0, false, true)
      defaultRaced.status.error shouldBe None
      defaultRaced.digest.height shouldBe second.height
      withClue(s"control scans ${defaultControl.scans}; raced scans ${defaultRaced.scans}: ") {
        defaultRaced.digest.walletBalance shouldBe defaultControl.digest.walletBalance
      }
      defaultRaced.scans.map(_.blockHeight) shouldBe Vector(1, 2)
      defaultRaced.scans.map(_.beforeHeight) shouldBe Vector(0, 1)
      defaultRaced.scans.forall(_.succeeded) shouldBe true

      val control = run("single-pass", 1, false, false)
      control.scans.map(_.blockHeight) shouldBe Vector(1, 2)
      control.scans.forall(_.succeeded) shouldBe true
      control.status.error shouldBe None
      control.digest.height shouldBe second.height
      control.digest.walletBalance should be > 0L

      val raced = run("queued-catch-up", 1, true, false)
      raced.scans.map(_.blockHeight) shouldBe Vector(1, 2)
      raced.scans.map(_.beforeHeight) shouldBe Vector(0, 1)
      raced.scans.forall(_.succeeded) shouldBe true
      raced.status.error shouldBe None
      raced.digest.height shouldBe second.height
      withClue(s"control scans ${control.scans}; raced scans ${raced.scans}: ") {
        raced.digest.walletBalance shouldBe control.digest.walletBalance
      }
    }
  }

}
