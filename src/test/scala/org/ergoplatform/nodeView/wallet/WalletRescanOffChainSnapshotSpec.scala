package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome.Accepted
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually

import java.io.{File, IOException}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class WalletRescanOffChainSnapshotSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("a holder mempool transaction survives active rescan and a no-event restart") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val confirmedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        confirmedBalance / 2,
        Array.empty,
        Map.empty
      )
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "off-chain-rescan-snapshot").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val scanEntered = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val result = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == first.id) {
            scanEntered.countDown()
            if (!releaseScan.await(30, TimeUnit.SECONDS)) {
              throw new IllegalStateException("timed out waiting for the holder transaction")
            }
          }
          result
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory, Some(w.nodeViewHolderRef)
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      val holderOffChainBalance = try {
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        scanEntered.await(10, TimeUnit.SECONDS) shouldBe true

        val submit = TestProbe()(w.actorSystem)
        submit.send(w.nodeViewHolderRef,
          LocallyGeneratedTransaction(UnconfirmedTransaction(spendingTx, None)))
        submit.expectMsgType[Accepted](10.seconds)
        getCurrentView.pool.contains(spendingTx.id) shouldBe true

        // The holder sends this message to its vault on acceptance. Deliver it
        // to this independently instrumented wallet while its replay is paused.
        probe.send(actor, ScanOffChain(spendingTx))
        val expected = eventually(timeout(10.seconds), interval(100.millis)) {
          val balance = getBalancesWithUnconfirmed.walletBalance
          balance should be < confirmedBalance
          balance
        }
        releaseScan.countDown()
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
          status.height shouldBe first.height
          await(reader.confirmedBalances).walletBalance shouldBe confirmedBalance
          await(reader.balancesWithUnconfirmed).walletBalance shouldBe expected
        }
        expected
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      // The transaction was accepted before this actor subscribed. There is no
      // new ScanOffChain or ChangedMempool event to repair its in-memory index.
      getCurrentView.pool.contains(spendingTx.id) shouldBe true
      val restarted = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings),
        selector, getHistory, Some(w.nodeViewHolderRef)
      )))
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      val restartedProbe = TestProbe()(w.actorSystem)
      restartedProbe.watch(restarted)
      try {
        restartedProbe.send(restarted, RescanWallet(0))
        restartedProbe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
          status.height shouldBe first.height
          await(restartedReader.confirmedBalances).walletBalance shouldBe confirmedBalance
          await(restartedReader.balancesWithUnconfirmed).walletBalance shouldBe holderOffChainBalance
        }
      } finally {
        restartedProbe.send(restarted, CloseWallet)
        restartedProbe.expectTerminated(restarted, 5.seconds)
      }
    }
  }

  property("a direct wallet rescan retains an off-chain scan ahead of a stale pool update") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val confirmedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        confirmedBalance / 2,
        Array.empty,
        Map.empty
      )
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      val stalePool = getCurrentView.pool.getReader

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "off-chain-rescan-without-holder-ref").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val scanEntered = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val result = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == first.id) {
            scanEntered.countDown()
            if (!releaseScan.await(30, TimeUnit.SECONDS)) {
              throw new IllegalStateException("timed out waiting for the holder transaction")
            }
          }
          result
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        // This actor subscribed after the block was applied. Give the direct
        // constructor its current state reader before starting the rescan.
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error shouldBe None

        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        scanEntered.await(10, TimeUnit.SECONDS) shouldBe true

        val submit = TestProbe()(w.actorSystem)
        submit.send(w.nodeViewHolderRef,
          LocallyGeneratedTransaction(UnconfirmedTransaction(spendingTx, None)))
        val accepted = submit.expectMsgType[Accepted](10.seconds)
        accepted.tx.transaction.id shouldBe spendingTx.id
        getCurrentView.pool.contains(spendingTx.id) shouldBe true
        probe.send(actor, ScanOffChain(spendingTx))
        // A delayed pool notification can describe the view before acceptance.
        // Same-sender ordering puts it after the one-shot scan in this mailbox.
        probe.send(actor, ChangedMempool(stalePool))

        val expected = eventually(timeout(10.seconds), interval(100.millis)) {
          val balance = getBalancesWithUnconfirmed.walletBalance
          balance should be < confirmedBalance
          balance
        }
        releaseScan.countDown()
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
          status.height shouldBe first.height
          await(reader.confirmedBalances).walletBalance shouldBe confirmedBalance
          await(reader.balancesWithUnconfirmed).walletBalance shouldBe expected
        }
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a deferred off-chain scan is not counted again after selected-chain confirmation") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        initialBalance / 2,
        Array.empty,
        Map.empty
      )
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "off-chain-rescan-confirmed").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val scanEntered = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val result = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == first.id) {
            scanEntered.countDown()
            if (!releaseScan.await(45, TimeUnit.SECONDS)) {
              throw new IllegalStateException("timed out waiting for selected-chain confirmation")
            }
          }
          result
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error shouldBe None
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        scanEntered.await(10, TimeUnit.SECONDS) shouldBe true

        val submit = TestProbe()(w.actorSystem)
        submit.send(w.nodeViewHolderRef,
          LocallyGeneratedTransaction(UnconfirmedTransaction(spendingTx, None)))
        submit.expectMsgType[Accepted](10.seconds).tx.transaction.id shouldBe spendingTx.id
        getCurrentView.pool.contains(spendingTx.id) shouldBe true
        probe.send(actor, ScanOffChain(spendingTx))

        val second = makeNextBlock(getUtxoState, Seq(spendingTx))
        applyBlock(second) shouldBe 'success
        eventually(timeout(10.seconds), interval(100.millis)) {
          getCurrentView.history.ifHolderAppliedFullTip(second.id)(true) shouldBe Some(true)
        }
        // A late copy of the same notification must not put the now-confirmed
        // output back into the reconstructed off-chain registry.
        probe.send(actor, ScanOffChain(spendingTx))
        val confirmedAtSecond = eventually(timeout(10.seconds), interval(100.millis)) {
          val confirmed = getConfirmedBalances
          confirmed.height shouldBe second.height
          confirmed.walletBalance should be < initialBalance
          getBalancesWithUnconfirmed.walletBalance shouldBe confirmed.walletBalance
          confirmed.walletBalance
        }
        releaseScan.countDown()
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
          status.height shouldBe second.height
          await(reader.confirmedBalances).walletBalance shouldBe confirmedAtSecond
          await(reader.balancesWithUnconfirmed).walletBalance shouldBe confirmedAtSecond
        }
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a same-height retry keeps a deferred scan after a fail-closed stop") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val confirmedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        confirmedBalance / 2,
        Array.empty,
        Map.empty
      )
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      // The pool remains empty, so the deferred ScanOffChain is the only
      // source available to the direct actor after its same-height retry.
      getCurrentView.pool.contains(spendingTx.id) shouldBe false
      wallet.scanOffchain(spendingTx)
      val expected = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getBalancesWithUnconfirmed.walletBalance
        balance should be < confirmedBalance
        balance
      }

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "off-chain-rescan-same-height-retry").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val scanEntered = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val pauseFirstScan = new AtomicBoolean(true)
      val deferredSeen = new AtomicBoolean(false)
      val failFirstCompletion = new AtomicBoolean(true)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val result = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == first.id && pauseFirstScan.compareAndSet(true, false)) {
            scanEntered.countDown()
            if (!releaseScan.await(30, TimeUnit.SECONDS)) {
              throw new IllegalStateException("timed out waiting for the deferred scan")
            }
          }
          result
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      ) {
        override protected[wallet] def deferRescanOffChain(state: ErgoWalletState,
                                                           tx: ErgoTransaction): Unit = {
          super.deferRescanOffChain(state, tx)
          if (tx.id == spendingTx.id) deferredSeen.set(true)
        }

        override protected[wallet] def prepareRescanOffChainForCompletion(
            state: ErgoWalletState): Try[ErgoWalletState] = {
          if (failFirstCompletion.compareAndSet(true, false)) {
            Failure(new IOException("injected first off-chain completion failure"))
          } else super.prepareRescanOffChainForCompletion(state)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error shouldBe None
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        scanEntered.await(10, TimeUnit.SECONDS) shouldBe true
        probe.send(actor, ScanOffChain(spendingTx))
        releaseScan.countDown()

        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.exists(_.contains("injected first off-chain completion failure")) shouldBe true
        }
        deferredSeen.get() shouldBe true
        getCurrentView.pool.contains(spendingTx.id) shouldBe false

        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
          status.height shouldBe first.height
          await(reader.confirmedBalances).walletBalance shouldBe confirmedBalance
          await(reader.balancesWithUnconfirmed).walletBalance shouldBe expected
        }
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a same-height retry keeps an off-chain scan received after a fail-closed stop") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val confirmedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        confirmedBalance / 2,
        Array.empty,
        Map.empty
      )
      val spendingTx = await(wallet.generateTransaction(Seq(payment))).get
      getCurrentView.pool.contains(spendingTx.id) shouldBe false
      wallet.scanOffchain(spendingTx)
      val expected = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getBalancesWithUnconfirmed.walletBalance
        balance should be < confirmedBalance
        balance
      }

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "off-chain-rescan-post-stop-retry").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val failFirstCompletion = new AtomicBoolean(true)
      val service = new ErgoWalletServiceImpl(actorSettings)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      ) {
        override protected[wallet] def prepareRescanOffChainForCompletion(
            state: ErgoWalletState): Try[ErgoWalletState] = {
          if (failFirstCompletion.compareAndSet(true, false)) {
            Failure(new IOException("injected first off-chain completion failure"))
          } else super.prepareRescanOffChainForCompletion(state)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error shouldBe None
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.exists(_.contains("injected first off-chain completion failure")) shouldBe true
        }
        getCurrentView.pool.contains(spendingTx.id) shouldBe false

        // The one-shot notice arrives after the scan has stopped. Same-sender
        // ordering ensures the retry cannot overtake this quarantined message.
        probe.send(actor, ScanOffChain(spendingTx))
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
          status.height shouldBe first.height
          await(reader.confirmedBalances).walletBalance shouldBe confirmedBalance
          await(reader.balancesWithUnconfirmed).walletBalance shouldBe expected
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a no-holder retry remains fenced after the deferred scan buffer overflows") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val confirmedBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      def payment(amount: Long): PaymentRequest = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), amount, Array.empty, Map.empty
      )
      val firstTx = await(wallet.generateTransaction(Seq(payment(confirmedBalance / 2)))).get
      val overflowTx = await(wallet.generateTransaction(Seq(payment(confirmedBalance / 3)))).get
      firstTx.id should not be overflowTx.id
      getCurrentView.pool.contains(firstTx.id) shouldBe false
      getCurrentView.pool.contains(overflowTx.id) shouldBe false

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "off-chain-rescan-overflow-retry").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1, mempoolCapacity = 1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val failFirstCompletion = new AtomicBoolean(true)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      ) {
        override protected[wallet] def prepareRescanOffChainForCompletion(
            state: ErgoWalletState): Try[ErgoWalletState] = {
          if (failFirstCompletion.compareAndSet(true, false)) {
            Failure(new IOException("injected first off-chain completion failure"))
          } else super.prepareRescanOffChainForCompletion(state)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error shouldBe None
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.exists(_.contains("injected first off-chain completion failure")) shouldBe true
        }

        probe.send(actor, ScanOffChain(firstTx))
        probe.send(actor, ScanOffChain(overflowTx))
        probe.send(actor, GetWalletStatus)
        val stopped = probe.expectMsgType[WalletStatus](5.seconds)
        stopped.rescanState shouldBe WalletRescanState.NeedsRecovery
        stopped.error.exists(_.contains("deferred off-chain transaction capacity exceeded")) shouldBe true

        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.exists(_.contains("deferred off-chain transaction capacity exceeded")) shouldBe true
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }
}
