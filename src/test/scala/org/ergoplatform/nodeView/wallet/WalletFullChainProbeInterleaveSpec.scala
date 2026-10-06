package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.core.{idToVersion, versionToId}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, ChangedState}
import org.ergoplatform.nodeView.UtxoNodeViewHolder
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.history.HistorySectionFault
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.generators.{ErgoNodeTransactionGenerators, ValidBlocksGenerators}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.File
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class WalletFullChainProbeInterleaveSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private case class ActivationContext(contextBytes: Vector[Byte],
                                       stateVersion: Option[ModifierId],
                                       mempoolCurrent: Boolean,
                                       utxoVersion: Option[ModifierId],
                                       pendingRescan: Boolean)

  private case object ShortenSelectedTip

  property("a startup quarantine probe cannot accept a fresh partial rescan") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val history = getHistory
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "startup-quarantine-partial-rescan").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )
      val secondTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      history.bestFullBlockIdOpt shouldBe Some(second.id)

      val marker = WalletStorage.readOrCreate(actorSettings)
      try marker.quarantineDeepFork().get finally marker.close()

      val observations = TestProbe()(w.actorSystem)
      val reopened = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        override protected def probeSelectedFullChainBodies(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe = {
          observations.ref ! "probe pending"
          FullChainUnknown
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = reopened }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(reopened)
      try {
        observations.expectMsg("probe pending")
        await(reader.rescanWallet(2)).isFailure shouldBe true
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        probe.send(reopened, CloseWallet)
        probe.expectTerminated(reopened, 5.seconds)
      }

      val preserved = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        preserved.storage.deepForkQuarantine.get shouldBe true
        preserved.storage.pendingRescanStartHeight.get shouldBe None
        preserved.registry.committedVersionAndDigest.get._1 shouldBe first.id
      } finally {
        preserved.storage.close()
        preserved.registry.close()
      }
    }
  }

  property("catch-up does not advance past a selected body lost after preflight") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "catch-up-body-lost-after-preflight").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(seedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )
      val secondTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      val history = getHistory
      val appliedState = getCurrentState
      val observations = TestProbe()(w.actorSystem)
      val reopened = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        private var removed = false
        override protected def probeSelectedFullChainBodies(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe = {
          val result = super.probeSelectedFullChainBodies(targetId, targetHeight, cursor)
          if (!removed && result == FullChainSelected(third.id)) {
            removed = true
            HistorySectionFault.removeTransactionSection(history, second.header.transactionsId)
            observations.ref ! result
          }
          result
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = reopened }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(reopened)
      try {
        probe.send(reopened, ChangedState(appliedState))
        probe.send(reopened, ScanOnChain(third))
        observations.expectMsg(FullChainSelected(third.id))
        history.bestFullBlockAt(second.height) shouldBe None
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("quarantine")
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
        await(reader.rescanWallet(0)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.error.value.toLowerCase should include("missing")
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
        }
      } finally {
        probe.send(reopened, CloseWallet)
        probe.expectTerminated(reopened, 5.seconds)
      }
      val preserved = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        preserved.registry.committedVersionAndDigest.get._1 shouldBe first.id
        preserved.storage.rescanRecoveryIntent.get shouldBe true
      } finally {
        preserved.registry.close()
        preserved.storage.close()
      }
      val restarted = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      val restartProbe = TestProbe()(w.actorSystem)
      restartProbe.watch(restarted)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("rescan recovery is incomplete")
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
      } finally {
        restartProbe.send(restarted, CloseWallet)
        restartProbe.expectTerminated(restarted, 5.seconds)
      }
    }
  }

  property("a loaded wallet rescan from genesis does not skip a missing selected interior body") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success

      val history = getHistory
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "loaded-genesis-rescan-missing-interior-body").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, ScanOnChain(third))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
        }
        await(reader.confirmedBalances).walletBalance should be < initialBalance
        await(reader.rescanWallet(0)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
          await(reader.confirmedBalances).walletBalance should be < initialBalance
        }
        await(reader.rescanWallet(third.height + 1)) match {
          case Failure(_: RescanStartInvalid) => succeed
          case other => fail(s"Expected invalid rescan start, got $other")
        }
        await(reader.getWalletStatus).height shouldBe third.height

        HistorySectionFault.removeTransactionSection(history, second.header.transactionsId)
        history.bestFullBlockAt(second.height) shouldBe None
        history.bestFullBlockOpt.map(_.id) shouldBe Some(third.id)

        await(reader.rescanWallet(0)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error.value.toLowerCase should include("missing")
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a loaded suffix rescan and its restart recovery ignore only a pruned prefix") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      val history = getHistory
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "loaded-suffix-rescan-pruned-prefix").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def open() = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings),
        selector, history, Some(w.nodeViewHolderRef)
      )))
      def close(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val loaded = open()
      val loadedReader = new ErgoWalletReader { override val walletActor = loaded }
      try {
        loaded ! ChangedState(getCurrentState)
        loaded ! ScanOnChain(third)
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(loadedReader.getWalletStatus).height shouldBe third.height
        }
        HistorySectionFault.removeTransactionSection(history, first.header.transactionsId)
        history.bestFullBlockAt(first.height) shouldBe None
        history.bestFullBlockOpt.map(_.id) shouldBe Some(third.id)
        await(loadedReader.rescanWallet(2)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(loadedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
          await(loadedReader.confirmedBalances).height shouldBe third.height
        }
      } finally close(loaded)

      // Crash after quarantine clears but before the typed intent clears: the
      // remaining intent must fence reads and preserve the requested suffix.
      val storage = WalletStorage.readOrCreate(actorSettings)
      try {
        storage.deepForkQuarantine.get shouldBe false
        storage.beginRescanRecovery(2).get
        storage.quarantineDeepFork().get
        storage.deepForkQuarantine.get shouldBe true
        storage.clearDeepForkQuarantine().get
        storage.deepForkQuarantine.get shouldBe false
        storage.pendingRescanStartHeight.get shouldBe Some(2)
      } finally storage.close()

      val restarted = open()
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error.value.toLowerCase should include("recovery")
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
        await(restartedReader.rescanWallet(3)).isFailure shouldBe true
        await(restartedReader.rescanWallet(2)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
          await(restartedReader.confirmedBalances).height shouldBe third.height
        }

        HistorySectionFault.removeTransactionSection(history, second.header.transactionsId)
        history.bestFullBlockAt(second.height) shouldBe None
        await(restartedReader.rescanWallet(2)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error.value.toLowerCase should include("missing")
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
      } finally close(restarted)
    }
  }

  property("a loaded rescan without an applied full tip fails before deleting its registry") {
    withFixture { implicit w =>
      val history = getHistory
      history.bestFullBlockIdOpt shouldBe None
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "loaded-rescan-no-full-tip").getAbsolutePath
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        await(reader.rescanWallet(0)).isFailure shouldBe true
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
      val preserved = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        preserved.registry.fetchDigest().height shouldBe 0
        preserved.storage.rescanRecoveryIntent.get shouldBe false
        preserved.storage.deepForkQuarantine.get shouldBe false
      } finally {
        preserved.registry.close()
        preserved.storage.close()
      }
    }
  }

  property("a second rescan cannot replace a pending selected-body probe") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val history = getHistory
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "duplicate-rescan-during-body-probe").getAbsolutePath
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val allowProbe = new AtomicBoolean(true)
      val injectPriorError = new AtomicBoolean(true)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        override protected[wallet] def activateWallet(newState: ErgoWalletState): Unit =
          if (injectPriorError.getAndSet(false))
            super.activateWallet(newState.copy(error = Some("Earlier wallet context write failed")))
          else super.activateWallet(newState)

        override protected def probeSelectedFullChainBodies(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe =
          if (allowProbe.get()) super.probeSelectedFullChainBodies(targetId, targetHeight, cursor)
          else FullChainUnknown
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe Some("Earlier wallet context write failed")
        }
        allowProbe.set(false)
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(reader.getWalletStatus).rescanState shouldBe WalletRescanState.InProgress
        }
        probe.send(actor, RescanWallet(1))
        probe.expectMsgPF(5.seconds) { case Failure(_: RescanStartConflict) => succeed }
        allowProbe.set(true)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
          await(reader.confirmedBalances).height shouldBe first.height
        }
        probe.expectNoMessage(100.millis)
      } finally {
        allowProbe.set(true)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a failed selected catch-up scan stays quarantined after restart") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "failed-selected-catch-up").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def open(service: ErgoWalletServiceImpl): akka.actor.ActorRef =
        w.actorSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, service, selector, getHistory, Some(w.nodeViewHolderRef)
        )))
      def close(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val seed = open(new ErgoWalletServiceImpl(actorSettings))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally close(seed)

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      val failing = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] =
          if (block.id == second.id) throw new IllegalStateException("injected selected scan failure")
          else super.scanBlockUpdate(state, block, dustLimit)
      }
      val actor = open(failing)
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, ScanOnChain(third))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("quarantine")
        }
      } finally close(actor)

      getHistory.bestFullBlockAt(second.height).map(_.id) shouldBe Some(second.id)
      val restarted = open(new ErgoWalletServiceImpl(actorSettings))
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("rescan recovery is incomplete")
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
        await(restartedReader.rescanWallet(third.height)).isFailure shouldBe true
        await(restartedReader.rescanWallet(0)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
          await(restartedReader.confirmedBalances).height shouldBe third.height
        }
      } finally close(restarted)
      val recovered = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        recovered.registry.committedVersionAndDigest.get._1 shouldBe third.id
        recovered.storage.rescanRecoveryIntent.get shouldBe false
        recovered.storage.deepForkQuarantine.get shouldBe false
      } finally {
        recovered.registry.close()
        recovered.storage.close()
      }
    }
  }

  property("a pending rescan intent fences a checkpoint already at the selected tip after restart") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "pending-rescan-intent-at-tip").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def open(service: ErgoWalletServiceImpl) = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory, Some(w.nodeViewHolderRef)
      )))
      def close(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val seed = open(new ErgoWalletServiceImpl(actorSettings))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      try {
        seed ! ScanOnChain(first)
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally close(seed)

      val storage = WalletStorage.readOrCreate(actorSettings)
      try {
        storage.deepForkQuarantine.get shouldBe false
        storage.beginRescanRecovery().get
      } finally storage.close()

      val failedAfterCommit = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val updated = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == first.id) {
            updated.get
            throw new IllegalStateException("injected failure after registry commit")
          }
          updated
        }
      }
      val interrupted = open(failedAfterCommit)
      val interruptedReader = new ErgoWalletReader { override val walletActor = interrupted }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(interruptedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("recovery")
        }
        Try(await(interruptedReader.confirmedBalances)).isFailure shouldBe true
        await(interruptedReader.rescanWallet(0)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(interruptedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("incomplete")
        }
      } finally close(interrupted)

      val pending = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        pending.registry.committedVersionAndDigest.get._1 shouldBe first.id
        pending.storage.rescanRecoveryIntent.get shouldBe true
        pending.storage.deepForkQuarantine.get shouldBe true
      } finally {
        pending.registry.close()
        pending.storage.close()
      }

      val restarted = open(new ErgoWalletServiceImpl(actorSettings))
      val reader = new ErgoWalletReader { override val walletActor = restarted }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error.value.toLowerCase should include("recovery")
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
        await(reader.rescanWallet(1)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          await(reader.confirmedBalances).height shouldBe first.height
        }
      } finally close(restarted)
    }
  }

  property("an explicit genesis rescan follows a same-branch holder tip extension before clearing intent") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rescan-same-branch-tip-growth").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }

        val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
          Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
          Array.empty, Map.empty
        )))).get
        val second = makeNextBlock(getUtxoState, Seq(secondTx))
        applyBlock(second) shouldBe 'success
        seedProbe.send(seed, ChangedState(getCurrentState))
        seedProbe.send(seed, ScanOnChain(second))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe second.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val second = getHistory.bestFullBlockOpt.get
      val appliedAtSecond = getCurrentState
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      val history = getHistory
      val enteredSecond = new CountDownLatch(1)
      val releaseSecond = new CountDownLatch(1)
      val scanned = TestProbe()(w.actorSystem)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (committed.isSuccess && block.id == second.id) {
            scanned.ref ! ((block.id, state.storage.rescanRecoveryIntent.get))
            enteredSecond.countDown()
            require(releaseSecond.await(15, TimeUnit.SECONDS), "test did not release rescan")
          } else if (committed.isSuccess && block.id == third.id) {
            scanned.ref ! ((block.id, state.storage.rescanRecoveryIntent.get))
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecond))
        await(reader.rescanWallet(0)).isSuccess shouldBe true
        enteredSecond.await(5, TimeUnit.SECONDS) shouldBe true
        scanned.expectMsg((second.id, true))

        val advanced = scala.concurrent.Future { applyBlock(third) }(
          scala.concurrent.ExecutionContext.global)
        scala.concurrent.Await.result(advanced, 5.seconds) shouldBe 'success
        eventually(timeout(10.seconds), interval(100.millis)) {
          getCurrentState.version shouldBe idToVersion(third.id)
          history.bestFullBlockIdOpt shouldBe Some(third.id)
          history.ifHolderAppliedFullTip(third.id)(true) shouldBe Some(true)
        }
        releaseSecond.countDown()

        scanned.expectMsg(10.seconds, (third.id, true))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
        }
      } finally {
        releaseSecond.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val completed = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        completed.registry.committedVersionAndDigest.get._1 shouldBe third.id
        completed.storage.rescanRecoveryIntent.get shouldBe false
        completed.storage.deepForkQuarantine.get shouldBe false
      } finally {
        completed.registry.close()
        completed.storage.close()
      }
    }
  }

  property("quarantined state and mempool updates reach signing context before rescan activation") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val appliedAtFirst = getCurrentState
      val firstContextBytes = appliedAtFirst.stateContext.bytes.toVector
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rescan-quarantined-context-events").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ChangedState(appliedAtFirst))
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val appliedAtSecond = getCurrentState
      val secondContextBytes = appliedAtSecond.stateContext.bytes.toVector
      secondContextBytes should not be firstContextBytes
      val pool = ErgoMemPool.empty(actorSettings)
      val enteredFirst = new CountDownLatch(1)
      val releaseFirst = new CountDownLatch(1)
      val activations = TestProbe()(w.actorSystem)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (committed.isSuccess && block.id == first.id) {
            enteredFirst.countDown()
            require(releaseFirst.await(15, TimeUnit.SECONDS), "test did not release genesis replay")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      ) {
        override protected[wallet] def activateWallet(newState: ErgoWalletState): Unit = {
          activations.ref ! ActivationContext(
            newState.stateContext.bytes.toVector,
            newState.stateReaderOpt.map(reader => versionToId(reader.version)),
            newState.mempoolReaderOpt.exists(_ eq pool),
            newState.utxoStateReaderOpt.map(reader => versionToId(reader.version)),
            newState.storage.rescanRecoveryIntent.get
          )
          super.activateWallet(newState)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        val before = activations.expectMsgType[ActivationContext](5.seconds)
        before.contextBytes shouldBe firstContextBytes
        before.stateVersion shouldBe None
        before.mempoolCurrent shouldBe false
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }

        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        enteredFirst.await(5, TimeUnit.SECONDS) shouldBe true
        probe.send(actor, ChangedState(appliedAtSecond))
        probe.send(actor, ChangedMempool(pool))
        probe.send(actor, GetWalletStatus)
        releaseFirst.countDown()
        val queued = probe.expectMsgType[WalletStatus](10.seconds)
        queued.rescanState shouldBe WalletRescanState.InProgress

        val activated = activations.expectMsgType[ActivationContext](10.seconds)
        activated.contextBytes shouldBe secondContextBytes
        activated.stateVersion shouldBe Some(second.id)
        activated.mempoolCurrent shouldBe true
        activated.utxoVersion shouldBe Some(second.id)
        activated.pendingRescan shouldBe false
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe second.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
        }
      } finally {
        releaseFirst.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a restarted rescan refreshes signing context from the holder without change events") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val appliedAtFirst = getCurrentState
      val firstContextBytes = appliedAtFirst.stateContext.bytes.toVector
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rescan-restart-holder-context").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ChangedState(appliedAtFirst))
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      // getCurrentView asks the real holder with GetDataFromCurrentView. The
      // reopened wallet subscribes only after this state transition has ended.
      val holderView = eventually(timeout(10.seconds), interval(100.millis)) {
        val view = getCurrentView
        view.state.version shouldBe idToVersion(second.id)
        view.history.ifHolderAppliedFullTip(second.id)(true) shouldBe Some(true)
        view
      }
      val holderContextBytes = holderView.state.stateContext.bytes.toVector
      holderContextBytes should not be firstContextBytes
      val holderPoolReader = holderView.pool.getReader
      val persistedBefore = WalletStorage.readOrCreate(actorSettings)
      try persistedBefore.readStateContext(parameters).bytes.toVector shouldBe firstContextBytes
      finally persistedBefore.close()

      val activations = TestProbe()(w.actorSystem)
      val observeRescanActivation = new AtomicBoolean(false)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings),
        selector, getHistory, Some(w.nodeViewHolderRef)
      ) {
        override protected[wallet] def activateWallet(newState: ErgoWalletState): Unit = {
          if (observeRescanActivation.get()) {
            activations.ref ! ActivationContext(
              newState.stateContext.bytes.toVector,
              newState.stateReaderOpt.map(reader => versionToId(reader.version)),
              newState.mempoolReaderOpt.exists(_ eq holderPoolReader),
              newState.utxoStateReaderOpt.map(reader => versionToId(reader.version)),
              newState.storage.rescanRecoveryIntent.get
            )
          }
          super.activateWallet(newState)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
        observeRescanActivation.set(true)
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))

        val activated = activations.expectMsgType[ActivationContext](10.seconds)
        activated.contextBytes shouldBe holderContextBytes
        activated.stateVersion shouldBe Some(second.id)
        activated.mempoolCurrent shouldBe true
        activated.utxoVersion shouldBe Some(second.id)
        activated.pendingRescan shouldBe false
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe second.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val persistedAfter = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        persistedAfter.registry.committedVersionAndDigest.get._1 shouldBe second.id
        persistedAfter.storage.readStateContext(parameters).bytes.toVector shouldBe holderContextBytes
        persistedAfter.storage.rescanRecoveryIntent.get shouldBe false
      } finally {
        persistedAfter.registry.close()
        persistedAfter.storage.close()
      }
    }
  }

  property("a direct rescan retry uses a state event received after recovery stopped") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        getCurrentState.version shouldBe idToVersion(first.id)
        balance
      }
      val appliedAtFirst = getCurrentState
      val firstContextBytes = appliedAtFirst.stateContext.bytes.toVector
      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rescan-stopped-state-event-retry").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val failFirstCheckpoint = new AtomicBoolean(true)
      val observeRetryActivation = new AtomicBoolean(false)
      val activations = TestProbe()(w.actorSystem)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      ) {
        override protected[wallet] def syncRescanRegistryCheckpoint(
          state: ErgoWalletState,
          tip: ModifierId,
          height: Int
        ): Try[Unit] =
          if (failFirstCheckpoint.getAndSet(false))
            Failure(new IllegalStateException("injected H1 checkpoint sync failure"))
          else super.syncRescanRegistryCheckpoint(state, tip, height)

        override protected[wallet] def activateWallet(newState: ErgoWalletState): Unit = {
          if (observeRetryActivation.get()) {
            activations.ref ! ActivationContext(
              newState.stateContext.bytes.toVector,
              newState.stateReaderOpt.map(reader => versionToId(reader.version)),
              false,
              newState.utxoStateReaderOpt.map(reader => versionToId(reader.version)),
              newState.storage.rescanRecoveryIntent.get
            )
          }
          super.activateWallet(newState)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtFirst))
        probe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(reader.getWalletStatus).height shouldBe first.height
        }
        // Control delivery: the H2 state update below is the only one this
        // direct actor can receive after the failed H1 recovery.
        w.actorSystem.eventStream.unsubscribe(actor, classOf[ChangedState])
        probe.send(actor, RescanWallet(0))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.value should include("injected H1 checkpoint sync failure")
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true

        applyBlock(second) shouldBe 'success
        val appliedAtSecond = eventually(timeout(10.seconds), interval(100.millis)) {
          val current = getCurrentState
          current.version shouldBe idToVersion(second.id)
          getHistory.ifHolderAppliedFullTip(second.id)(true) shouldBe Some(true)
          current
        }
        val secondContextBytes = appliedAtSecond.stateContext.bytes.toVector
        secondContextBytes should not be firstContextBytes
        probe.send(actor, ChangedState(appliedAtSecond))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).rescanState shouldBe WalletRescanState.NeedsRecovery

        observeRetryActivation.set(true)
        probe.send(actor, RescanWallet(1))
        probe.expectMsg(5.seconds, Success(()))
        val activated = activations.expectMsgType[ActivationContext](10.seconds)
        activated.contextBytes shouldBe secondContextBytes
        activated.stateVersion shouldBe Some(second.id)
        activated.pendingRescan shouldBe false
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe second.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
          await(reader.confirmedBalances).height shouldBe second.height
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val persisted = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        persisted.registry.committedVersionAndDigest.get._1 shouldBe second.id
        persisted.storage.readStateContext(parameters).bytes.toVector shouldBe
          getCurrentState.stateContext.bytes.toVector
        persisted.storage.rescanRecoveryIntent.get shouldBe false
        persisted.storage.deepForkQuarantine.get shouldBe false
      } finally {
        persisted.registry.close()
        persisted.storage.close()
      }
    }
  }

  property("a holder rollback after rescan commits its tip leaves durable recovery instead of hanging") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rescan-shorter-holder-tip-after-commit").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      eventually(timeout(10.seconds), interval(100.millis)) {
        getCurrentState.version shouldBe idToVersion(second.id)
        getHistory.ifHolderAppliedFullTip(second.id)(true) shouldBe Some(true)
      }

      // Replace the fixture holder only after it has applied H2. The test
      // subclass keeps real History, UTXO State, and holder installation logic.
      val vaultProbe = TestProbe()(w.actorSystem)
      vaultProbe.watch(w.wallet.walletActor)
      vaultProbe.send(w.wallet.walletActor, CloseWallet)
      vaultProbe.expectTerminated(w.wallet.walletActor, 5.seconds)
      w.stopNodeViewHolder()
      w.nodeViewHolderRef = w.actorSystem.actorOf(Props(new UtxoNodeViewHolder(w.settings) {
        private def shortening: Receive = {
          case ShortenSelectedTip =>
            val moved = for {
              selected <- history().reportModifierIsInvalid(second.header,
                ProgressInfo[BlockSection](Some(first.id), Seq(second), Seq.empty, Seq.empty))
              rolled <- minimalState().rollbackTo(idToVersion(first.id))
            } yield {
              updateNodeView(updatedHistory = Some(selected._1), updatedState = Some(rolled))
              first.id
            }
            sender() ! moved
        }

        override def receive: Receive = shortening orElse super.receive
      }))
      eventually(timeout(10.seconds), interval(100.millis)) {
        val view = getCurrentView
        view.state.version shouldBe idToVersion(second.id)
        view.history.ifHolderAppliedFullTip(second.id)(true) shouldBe Some(true)
      }

      val committedScan = TestProbe()(w.actorSystem)
      val releaseSecond = new CountDownLatch(1)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (committed.isSuccess && block.id == second.id) {
            committedScan.ref ! ((state.registry.committedVersionAndDigest.get._1,
              state.storage.rescanRecoveryIntent.get))
            require(releaseSecond.await(15, TimeUnit.SECONDS), "test did not release H2 replay")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory, Some(w.nodeViewHolderRef)
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      val holderProbe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
        probe.send(actor, RescanWallet(2))
        probe.expectMsg(5.seconds, Success(()))
        committedScan.expectMsg(10.seconds, (second.id, true))

        holderProbe.send(w.nodeViewHolderRef, ShortenSelectedTip)
        holderProbe.expectMsg(5.seconds, Success(first.id))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val view = getCurrentView
          view.state.version shouldBe idToVersion(first.id)
          view.history.bestFullBlockIdOpt shouldBe Some(first.id)
          view.history.ifHolderAppliedFullTip(first.id)(true) shouldBe Some(true)
        }

        // The direct-actor fixture forwards the holder rollback notification
        // that the production wallet wrapper receives from the node view.
        probe.send(actor, Rollback(idToVersion(first.id)))
        probe.send(actor, GetWalletStatus)
        releaseSecond.countDown()
        val terminal = probe.expectMsgType[WalletStatus](10.seconds)
        terminal.rescanState shouldBe WalletRescanState.NeedsRecovery
        terminal.error.value.toLowerCase should include("rolled back")
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        releaseSecond.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val pending = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        pending.registry.committedVersionAndDigest.get._1 shouldBe second.id
        pending.storage.pendingRescanStartHeight.get shouldBe Some(2)
        pending.storage.rescanRecoveryIntent.get shouldBe true
        pending.storage.deepForkQuarantine.get shouldBe true
      } finally {
        pending.registry.close()
        pending.storage.close()
      }

      val holderContextBytes = getCurrentView.state.stateContext.bytes.toVector
      val restarted = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings),
        selector, getHistory, Some(w.nodeViewHolderRef)
      )))
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      val restartedProbe = TestProbe()(w.actorSystem)
      restartedProbe.watch(restarted)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(restartedReader.getWalletStatus).rescanState shouldBe WalletRescanState.NeedsRecovery
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true

        restartedProbe.send(restarted, RescanWallet(1))
        restartedProbe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
          await(restartedReader.confirmedBalances).height shouldBe first.height
        }
      } finally {
        restartedProbe.send(restarted, CloseWallet)
        restartedProbe.expectTerminated(restarted, 5.seconds)
      }

      val recovered = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        recovered.registry.committedVersionAndDigest.get._1 shouldBe first.id
        recovered.storage.readStateContext(parameters).bytes.toVector shouldBe holderContextBytes
        recovered.storage.rescanRecoveryIntent.get shouldBe false
        recovered.storage.deepForkQuarantine.get shouldBe false
      } finally {
        recovered.registry.close()
        recovered.storage.close()
      }
    }
  }

  property("a catch-up failure after committing the selected tip stays quarantined after restart") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "selected-catch-up-failed-after-tip-commit").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def open(service: ErgoWalletServiceImpl) = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      )))
      def close(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val seed = open(new ErgoWalletServiceImpl(actorSettings))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      try {
        seed ! ScanOnChain(first)
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally close(seed)

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      val failedAfterCommit = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val updated = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == third.id) {
            updated.get
            throw new IllegalStateException("injected catch-up failure after tip commit")
          }
          updated
        }
      }
      val actor = open(failedAfterCommit)
      val reader = new ErgoWalletReader { override val walletActor = actor }
      try {
        actor ! ChangedState(getCurrentState)
        actor ! ScanOnChain(third)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error.value.toLowerCase should include("quarantine")
        }
      } finally close(actor)

      val restarted = open(new ErgoWalletServiceImpl(actorSettings))
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error.value.toLowerCase should include("recovery")
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
      } finally close(restarted)
      val pending = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        pending.registry.committedVersionAndDigest.get._1 shouldBe third.id
        pending.storage.rescanRecoveryIntent.get shouldBe true
        pending.storage.deepForkQuarantine.get shouldBe true
      } finally {
        pending.registry.close()
        pending.storage.close()
      }
    }
  }

  property("catch-up scans the applied full branch when the best headers follow another fork") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "selected-full-branch-catch-up").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(secondA) shouldBe 'success
      val secondAExternal = secondA.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdATx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(secondAExternal), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val thirdA = makeNextBlock(getUtxoState, Seq(thirdATx))
      applyBlock(thirdA) shouldBe 'success
      val appliedState = getCurrentState
      val history = getHistory
      val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirst = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondB = ValidBlocksGenerators.validFullBlock(
        Some(first), forkAtFirst, Seq(secondTx), Some(first.header.timestamp + 101)
      )
      val forkAtSecondB = forkAtFirst.applyModifier(secondB)(_ => ()).get
      val secondBExternal = secondB.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(secondBExternal), stateCtxOpt = Some(forkAtSecondB.stateContext)
      )
      val thirdB = ValidBlocksGenerators.validFullBlock(Some(secondB), forkAtSecondB, Seq(thirdBTx))
      val forkAtThirdB = forkAtSecondB.applyModifier(thirdB)(_ => ()).get
      val thirdBExternal = thirdB.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val fourthBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(thirdBExternal), stateCtxOpt = Some(forkAtThirdB.stateContext)
      )
      val fourthB = ValidBlocksGenerators.validFullBlock(Some(thirdB), forkAtThirdB, Seq(fourthBTx))
      Seq(secondB, thirdB, fourthB).foreach(block => history.append(block.header).get)
      history.bestHeaderIdAtHeight(secondA.height) shouldBe Some(secondB.id)
      history.bestFullBlockIdOpt shouldBe Some(thirdA.id)
      history.bestFullBlockAt(secondA.height) shouldBe None

      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedState))
        probe.send(actor, ScanOnChain(thirdA))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error shouldBe None
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val emptySettings = actorSettings.copy(
        directory = new File(w.nodeViewDir, "empty-selected-full-branch-catch-up").getAbsolutePath
      )
      val emptyActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        emptySettings, parameters, new ErgoWalletServiceImpl(emptySettings), selector, history
      )))
      val emptyReader = new ErgoWalletReader { override val walletActor = emptyActor }
      val emptyProbe = TestProbe()(w.actorSystem)
      emptyProbe.watch(emptyActor)
      try {
        emptyProbe.send(emptyActor, ChangedState(appliedState))
        emptyProbe.send(emptyActor, ScanOnChain(thirdA))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(emptyReader.getWalletStatus)
          status.height shouldBe thirdA.height
          status.error shouldBe None
        }
      } finally {
        emptyProbe.send(emptyActor, CloseWallet)
        emptyProbe.expectTerminated(emptyActor, 5.seconds)
      }
    }
  }

  property("an empty registry does not skip a missing selected body during bootstrap catch-up") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      val history = getHistory
      val appliedState = getCurrentState
      HistorySectionFault.removeTransactionSection(history, second.header.transactionsId)
      history.bestFullBlockAt(second.height) shouldBe None
      history.bestFullBlockOpt.map(_.id) shouldBe Some(third.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "empty-registry-missing-catch-up").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedState))
        probe.send(actor, ScanOnChain(third))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe 0
          status.error.value.toLowerCase should include("quarantine")
        }
        await(reader.rescanWallet(0)).isSuccess shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe 0
          status.error.value.toLowerCase should include("missing")
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
      val preserved = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        preserved.registry.fetchDigest().height shouldBe 0
        preserved.storage.deepForkQuarantine.get shouldBe true
        preserved.storage.rescanRecoveryIntent.get shouldBe true
      } finally {
        preserved.registry.close()
        preserved.storage.close()
      }
    }
  }

  property("direct live scan failure after a registry commit fences reads and restart") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "direct-scan-failed-after-commit").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def open(service: ErgoWalletServiceImpl) = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      )))
      def close(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val seed = open(new ErgoWalletServiceImpl(actorSettings))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      try {
        seed ! ScanOnChain(first)
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally close(seed)

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val failedAfterCommit = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == second.id) {
            committed.get
            Failure(new IllegalStateException("injected direct failure after registry commit"))
          } else committed
        }
      }
      val failed = open(failedAfterCommit)
      val reader = new ErgoWalletReader { override val walletActor = failed }
      val probe = TestProbe()(w.actorSystem)
      try {
        probe.send(failed, ChangedState(getCurrentState))
        probe.send(failed, ScanOnChain(second))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe second.height
          status.error.value.toLowerCase should include("quarantine")
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
        }
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally close(failed)

      val pending = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        pending.registry.committedVersionAndDigest.get._1 shouldBe second.id
        pending.storage.rescanRecoveryIntent.get shouldBe true
        pending.storage.deepForkQuarantine.get shouldBe true
      } finally {
        pending.registry.close()
        pending.storage.close()
      }
      val restarted = open(new ErgoWalletServiceImpl(actorSettings))
      val restartedReader = new ErgoWalletReader { override val walletActor = restarted }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(restartedReader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.value.toLowerCase should include("recovery")
        }
        Try(await(restartedReader.confirmedBalances)).isFailure shouldBe true
      } finally close(restarted)
    }
  }

  property("history advances while a selected catch-up scan is held, then wallet follows the new tip") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "catch-up-scan-does-not-lock-history").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val appliedAtSecond = getCurrentState
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      val history = getHistory
      val enteredScan = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == second.id) {
            enteredScan.countDown()
            require(releaseScan.await(10, TimeUnit.SECONDS), "test did not release selected scan")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecond))
        probe.send(actor, ScanInThePast(second.height, false))
        enteredScan.await(5, TimeUnit.SECONDS) shouldBe true
        val advanced = scala.concurrent.Future { applyBlock(third) }(
          scala.concurrent.ExecutionContext.global)
        try {
          scala.concurrent.Await.result(advanced, 3.seconds) shouldBe 'success
          history.bestFullBlockIdOpt shouldBe Some(third.id)
        } finally releaseScan.countDown()
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
        }
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("an exact-tip catch-up request terminates and a stale legacy rescan cannot scan") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val history = getHistory
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "exact-tip-catch-up-stale-rescan").getAbsolutePath
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val scanCalls = new AtomicInteger(0)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          scanCalls.incrementAndGet()
          super.scanBlockUpdate(state, block, dustLimit)
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
        val completedScans = scanCalls.get()
        probe.send(actor, ScanInThePast(first.height + 1, false))
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
        scanCalls.get() shouldBe completedScans
        probe.send(actor, ScanInThePast(first.height, true))
        probe.send(actor, GetWalletStatus)
        val afterStale = probe.expectMsgType[WalletStatus](5.seconds)
        afterStale.height shouldBe first.height
        afterStale.error shouldBe None
        scanCalls.get() shouldBe completedScans
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a rival applied full tip after scan commit never exposes the old branch") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rival-tip-after-selected-scan-commit").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(secondA) shouldBe 'success
      val appliedAtSecondA = getCurrentState
      val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirst = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondB = ValidBlocksGenerators.validFullBlock(
        Some(first), forkAtFirst, Seq(secondTx), Some(first.header.timestamp + 101)
      )
      val forkAtSecondB = forkAtFirst.applyModifier(secondB)(_ => ()).get
      val secondBExternal = secondB.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(secondBExternal), stateCtxOpt = Some(forkAtSecondB.stateContext)
      )
      val thirdB = ValidBlocksGenerators.validFullBlock(Some(secondB), forkAtSecondB, Seq(thirdBTx))
      val history = getHistory
      val enteredScan = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val enteredOtherBranch = TestProbe()(w.actorSystem)
      val actorRestarted = new AtomicBoolean(false)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == secondA.id) {
            enteredScan.countDown()
            require(releaseScan.await(15, TimeUnit.SECONDS), "test did not release rival-tip scan")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      ) {
        override protected[wallet] def awaitSupersedingRollback(state: ErgoWalletState,
                                                                 detail: String): Unit = {
          enteredOtherBranch.ref ! detail
          super.awaitSupersedingRollback(state, detail)
        }
        override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
          actorRestarted.set(true)
          super.preRestart(reason, message)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecondA))
        probe.send(actor, ScanInThePast(secondA.height, false))
        enteredScan.await(5, TimeUnit.SECONDS) shouldBe true
        val rivalApplied = scala.concurrent.Future {
          applyBlock(secondB).flatMap(_ => applyBlock(thirdB))
        }(scala.concurrent.ExecutionContext.global)
        scala.concurrent.Await.result(rivalApplied, 5.seconds) shouldBe 'success
        history.bestFullBlockIdOpt shouldBe Some(thirdB.id)
        probe.send(actor, GetWalletStatus)
        releaseScan.countDown()
        val status = probe.expectMsgType[WalletStatus](10.seconds)
        status.error should not be empty
        enteredOtherBranch.expectMsgType[String](5.seconds).toLowerCase should include("off the selected")
        actorRestarted.get() shouldBe false
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a direct on-chain scan committed before a rival tip cannot expose the old branch") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rival-tip-after-direct-scan-commit").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(secondA) shouldBe 'success
      val appliedAtSecondA = getCurrentState
      val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirst = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondB = ValidBlocksGenerators.validFullBlock(
        Some(first), forkAtFirst, Seq(secondTx), Some(first.header.timestamp + 101)
      )
      val forkAtSecondB = forkAtFirst.applyModifier(secondB)(_ => ()).get
      val secondBExternal = secondB.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(secondBExternal), stateCtxOpt = Some(forkAtSecondB.stateContext)
      )
      val thirdB = ValidBlocksGenerators.validFullBlock(Some(secondB), forkAtSecondB, Seq(thirdBTx))
      val history = getHistory
      val enteredScan = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val enteredOtherBranch = TestProbe()(w.actorSystem)
      val actorRestarted = new AtomicBoolean(false)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == secondA.id) {
            enteredScan.countDown()
            require(releaseScan.await(15, TimeUnit.SECONDS), "test did not release direct scan")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      ) {
        override protected[wallet] def awaitSupersedingRollback(state: ErgoWalletState,
                                                                 detail: String): Unit = {
          enteredOtherBranch.ref ! detail
          super.awaitSupersedingRollback(state, detail)
        }
        override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
          actorRestarted.set(true)
          super.preRestart(reason, message)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecondA))
        probe.send(actor, ScanOnChain(secondA))
        enteredScan.await(5, TimeUnit.SECONDS) shouldBe true
        val rivalApplied = scala.concurrent.Future {
          applyBlock(secondB).flatMap(_ => applyBlock(thirdB))
        }(scala.concurrent.ExecutionContext.global)
        scala.concurrent.Await.result(rivalApplied, 5.seconds) shouldBe 'success
        history.bestFullBlockIdOpt shouldBe Some(thirdB.id)
        probe.send(actor, GetWalletStatus)
        releaseScan.countDown()
        val status = probe.expectMsgType[WalletStatus](10.seconds)
        status.error should not be empty
        enteredOtherBranch.expectMsgType[String](5.seconds).toLowerCase should include("off the selected")
        actorRestarted.get() shouldBe false
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a direct on-chain scan committed before a same-branch tip advances catches up") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "same-branch-after-direct-scan-commit").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val appliedAtSecond = getCurrentState
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      val history = getHistory
      val enteredScan = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val actorRestarted = new AtomicBoolean(false)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == second.id) {
            enteredScan.countDown()
            require(releaseScan.await(15, TimeUnit.SECONDS), "test did not release same-branch direct scan")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      ) {
        override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
          actorRestarted.set(true)
          super.preRestart(reason, message)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecond))
        probe.send(actor, ScanOnChain(second))
        enteredScan.await(5, TimeUnit.SECONDS) shouldBe true
        val advanced = scala.concurrent.Future { applyBlock(third) }(
          scala.concurrent.ExecutionContext.global)
        scala.concurrent.Await.result(advanced, 5.seconds) shouldBe 'success
        history.bestFullBlockIdOpt shouldBe Some(third.id)
        probe.send(actor, GetWalletStatus)
        releaseScan.countDown()
        probe.expectMsgType[WalletStatus](10.seconds).error should not be empty
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
        }
        actorRestarted.get() shouldBe false
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a queued rollback escapes an unknown proof when the applied full tip is shorter") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "direct-scan-shorter-applied-tip-rollback").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        seedProbe.send(seed, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(seedReader.getWalletStatus).height shouldBe first.height
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val appliedAtSecond = getCurrentState
      val history = getHistory
      val enteredScan = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val proofObserved = TestProbe()(w.actorSystem)
      val rollbackStarted = TestProbe()(w.actorSystem)
      val actorRestarted = new AtomicBoolean(false)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val committed = super.scanBlockUpdate(state, block, dustLimit)
          if (block.id == second.id) {
            enteredScan.countDown()
            require(releaseScan.await(15, TimeUnit.SECONDS), "test did not release shorter-tip scan")
          }
          committed
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      ) {
        override protected def probeSelectedFullChainBodies(targetId: ModifierId,
                                                            targetHeight: Int,
                                                            cursor: Option[FullChainCursor]): FullChainProbe = {
          val result = super.probeSelectedFullChainBodies(targetId, targetHeight, cursor)
          if (targetId == second.id) proofObserved.ref ! result
          result
        }
        override protected[wallet] def verifyRetainedRollback(state: ErgoWalletState,
                                                               version: org.ergoplatform.core.VersionTag): Unit = {
          rollbackStarted.ref ! version
          super.verifyRetainedRollback(state, version)
        }
        override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
          actorRestarted.set(true)
          super.preRestart(reason, message)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecond))
        probe.send(actor, ScanOnChain(second))
        enteredScan.await(5, TimeUnit.SECONDS) shouldBe true

        // Exercise real History and State rollback mechanics. The holder's
        // applied-version marker now names the shorter selected full tip.
        history.reportModifierIsInvalid(second.header,
          ProgressInfo[BlockSection](Some(first.id), Seq(second), Seq.empty, Seq.empty)).get
        val rolledState = getUtxoState.rollbackTo(idToVersion(first.id)).get
        rolledState.version shouldBe idToVersion(first.id)
        history.recordHolderAppliedStateVersion(rolledState.version)
        history.bestFullBlockIdOpt shouldBe Some(first.id)
        history.ifHolderAppliedFullTip(first.id)(true) shouldBe Some(true)
        history.appliedFullChainBodyProbe(second.id, second.height) shouldBe FullChainUnknown

        probe.send(actor, Rollback(idToVersion(first.id)))
        probe.send(actor, GetWalletStatus)
        releaseScan.countDown()
        probe.expectMsgType[WalletStatus](10.seconds).error should not be empty
        proofObserved.expectMsg(FullChainUnknown)
        rollbackStarted.expectMsg(idToVersion(first.id))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          await(reader.confirmedBalances).walletBalance shouldBe initialBalance
        }
        actorRestarted.get() shouldBe false
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("bootstrap arms and installs a selected scan plan under one applied-tip fence") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val secondTx = await(wallet.generateTransaction(Seq(PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), initialBalance / 2,
        Array.empty, Map.empty
      )))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val appliedAtSecond = getCurrentState
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "bootstrap-tip-change-before-replay").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val observations = TestProbe()(w.actorSystem)
      val attemptingHistory = new CountDownLatch(1)
      val enteredHistory = new CountDownLatch(1)
      val releaseScan = new CountDownLatch(1)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      ) {
        private var held = false
        override protected[wallet] def armSelectedCatchUpScan(tip: ModifierId,
                                                                startHeight: Int): Boolean = {
          val armed = super.armSelectedCatchUpScan(tip, startHeight)
          if (armed && !held) {
            held = true
            observations.ref ! tip
            require(releaseScan.await(10, TimeUnit.SECONDS), "test did not release selected bootstrap scan")
          }
          armed
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(appliedAtSecond))
        probe.send(actor, ScanOnChain(second))
        observations.expectMsg(second.id)
        val advanced = scala.concurrent.Future {
          attemptingHistory.countDown()
          getHistory.ifHolderAppliedFullTip(second.id) {
            enteredHistory.countDown()
          }
          applyBlock(third)
        }(scala.concurrent.ExecutionContext.global)
        attemptingHistory.await(5, TimeUnit.SECONDS) shouldBe true
        val enteredBeforeInstall = enteredHistory.await(1, TimeUnit.SECONDS)
        releaseScan.countDown()
        scala.concurrent.Await.result(advanced, 5.seconds) shouldBe 'success
        enteredBeforeInstall shouldBe false
        getHistory.bestFullBlockIdOpt shouldBe Some(third.id)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
        }
      } finally {
        releaseScan.countDown()
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }
    }
  }

  property("a missing replay body below the pruning floor is not a selected suffix") {
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
      val secondTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success

      val history = getHistory
      history.appliedFullChainBodyProbe(first.id, first.height) shouldBe
        FullChainSelected(third.id)
      history.writeMinimalFullBlockHeight(third.height)
      HistorySectionFault.removeTransactionSection(history, second.header.transactionsId)
      history.bestFullBlockAt(second.height) shouldBe None
      history.bestFullBlockOpt.map(_.id) shouldBe Some(third.id)
      history.appliedFullChainBodyProbe(first.id, first.height) shouldBe
        FullChainBodyMissing(third.id, second.height)
    }
  }

  property("a catch-up probe releases and resets a state event stashed across batches") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "catch-up-probe-stash").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val initialActor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val initialReader = new ErgoWalletReader { override val walletActor = initialActor }
      val initialProbe = TestProbe()(w.actorSystem)
      initialProbe.watch(initialActor)
      try {
        initialProbe.send(initialActor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(initialReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally {
        initialProbe.send(initialActor, CloseWallet)
        initialProbe.expectTerminated(initialActor, 5.seconds)
      }

      val payment = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder),
        initialBalance / 2,
        Array.empty,
        Map.empty
      )
      val secondTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success
      val external = second.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(external), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val third = makeNextBlock(getUtxoState, Seq(thirdTx))
      applyBlock(third) shouldBe 'success
      val history = getHistory
      val appliedState = getCurrentState
      val observations = TestProbe()(w.actorSystem)
      val reopened = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        private var bodyProbeCalls = 0

        override protected def probeSelectedFullChainBodies(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe = {
          bodyProbeCalls += 1
          val result = history.appliedFullChainBodyProbe(
            targetId, targetHeight, cursor, maxHeaders = 1
          )
          observations.ref ! result
          if (bodyProbeCalls == 1) self ! ChangedState(appliedState)
          result
        }

        override protected[wallet] def activeWallet(state: ErgoWalletState): Receive = {
          val normal = super.activeWallet(state)
          val observed: Receive = {
            case message @ ChangedState(changed) if changed.version == appliedState.version =>
              observations.ref ! (("released", pendingChainMessages))
              normal(message)
            case message if normal.isDefinedAt(message) => normal(message)
          }
          observed
        }
      }))
      val reopenedReader = new ErgoWalletReader { override val walletActor = reopened }
      val reopenedProbe = TestProbe()(w.actorSystem)
      reopenedProbe.watch(reopened)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reopenedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
        reopenedProbe.send(reopened, ScanOnChain(third))
        observations.expectMsgType[FullChainPending](5.seconds)
        observations.expectMsgType[FullChainPending](5.seconds)
        observations.expectMsg(FullChainSelected(third.id))
        observations.expectMsg(("released", 0))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reopenedReader.getWalletStatus)
          status.height shouldBe third.height
          status.error shouldBe None
        }
      } finally {
        reopenedProbe.send(reopened, CloseWallet)
        reopenedProbe.expectTerminated(reopened, 5.seconds)
      }
    }
  }

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
