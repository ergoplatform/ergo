package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletRegistry
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.{File, IOException}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class WalletRescanFailureCompositionSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("a failed explicit rescan cannot commit a later height before same-height recovery") {
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
      val history = getHistory
      history.bestFullBlockAt(first.height).map(_.id) shouldBe Some(first.id)
      history.bestFullBlockAt(second.height).map(_.id) shouldBe Some(second.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "failed-explicit-rescan-composition").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val failOnce = new AtomicBoolean(true)
      val failingService = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          if (block.id == first.id && failOnce.compareAndSet(true, false)) {
            Failure(new IllegalStateException("one-shot explicit rescan failure"))
          } else super.scanBlockUpdate(state, block, dustLimit)
        }
      }
      def open(service: ErgoWalletServiceImpl): ActorRef =
        w.actorSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, service, selector, history
        )))
      def close(actor: ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val failed = open(failingService)
      val failedReader = new ErgoWalletReader { override val walletActor = failed }
      val failedProbe = TestProbe()(w.actorSystem)
      try {
        failedProbe.send(failed, ChangedState(getCurrentState))
        failedProbe.send(failed, RescanWallet(first.height))
        failedProbe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(failedReader.getWalletStatus)
          status.height shouldBe 0
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.value should include("one-shot explicit rescan failure")
        }
        // The second status request is ordered behind any scan self-send.
        failedProbe.send(failed, GetWalletStatus)
        val settled = failedProbe.expectMsgType[WalletStatus](5.seconds)
        settled.height shouldBe 0
        settled.rescanState shouldBe WalletRescanState.NeedsRecovery
      } finally close(failed)

      val pending = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        pending.registry.committedVersionAndDigest.get._2.height shouldBe 0
        pending.registry.allWalletTxs() shouldBe empty
        pending.storage.pendingRescanStartHeight.get shouldBe Some(first.height)
      } finally {
        pending.registry.close()
        pending.storage.close()
      }

      val retry = open(new ErgoWalletServiceImpl(actorSettings))
      val retryReader = new ErgoWalletReader { override val walletActor = retry }
      val retryProbe = TestProbe()(w.actorSystem)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(retryReader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.height shouldBe 0
        }
        // A direct test actor has no holder to supply the current state view.
        // The retry must receive that reader before it can sync its final context.
        retryProbe.send(retry, ChangedState(getCurrentState))
        retryProbe.send(retry, RescanWallet(first.height))
        retryProbe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(retryReader.getWalletStatus)
          status.height shouldBe second.height
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
        }
      } finally close(retry)

      val recovered = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        val (tip, digest) = recovered.registry.committedVersionAndDigest.get
        tip shouldBe second.id
        digest.height shouldBe second.height
        recovered.registry.allWalletTxs().exists(_.tx.id == first.transactions.head.id) shouldBe true
        recovered.storage.rescanRecoveryIntent.get shouldBe false
      } finally {
        recovered.registry.close()
        recovered.storage.close()
      }
    }
  }

  property("a failed final registry sync leaves the rescan intent and balance fence durable") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "failed-rescan-checkpoint-sync").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val barrierReached = new AtomicBoolean(false)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      ) {
        override protected[wallet] def syncRescanRegistryCheckpoint(state: ErgoWalletState,
                                                                   tip: ModifierId,
                                                                   height: Int): Try[Unit] = {
          val (committedTip, digest) = state.registry.committedVersionAndDigest.get
          committedTip shouldBe first.id
          digest.height shouldBe first.height
          tip shouldBe first.id
          height shouldBe first.height
          state.storage.pendingRescanStartHeight.get shouldBe Some(first.height)
          state.storage.deepForkQuarantine.get shouldBe true
          barrierReached.set(true)
          Failure(new IOException("injected final registry sync failure"))
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, RescanWallet(first.height))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.value should include("injected final registry sync failure")
        }
        barrierReached.get() shouldBe true
        Try(await(reader.confirmedBalances)).isFailure shouldBe true
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.registry.committedVersionAndDigest.get._1 shouldBe first.id
        reopened.storage.pendingRescanStartHeight.get shouldBe Some(first.height)
        reopened.storage.deepForkQuarantine.get shouldBe true
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }
    }
  }

  property("a read failure after registry recreation retains the rebuilt handle for recovery") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "failed-post-recreate-wallet-read").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val recreateCalls = new AtomicInteger(0)
      val markersBeforeRecreate = new AtomicBoolean(false)
      val failNextRead = new AtomicBoolean(false)
      val faultTriggered = new AtomicBoolean(false)
      val firstRebuilt = new AtomicReference[ErgoWalletState]()
      val retryInput = new AtomicReference[ErgoWalletState]()
      val retryRebuilt = new AtomicReference[ErgoWalletState]()
      val failedActorState = new AtomicReference[ErgoWalletState]()
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def recreateRegistry(state: ErgoWalletState,
                                      settings: ErgoSettings): Try[ErgoWalletState] = {
          val call = recreateCalls.incrementAndGet()
          if (call == 1) {
            markersBeforeRecreate.set(
              state.storage.pendingRescanStartHeight.get.contains(first.height) &&
                state.storage.deepForkQuarantine.get
            )
          } else if (call == 2) retryInput.set(state)
          val result = super.recreateRegistry(state, settings)
          result.foreach { rebuilt =>
            if (call == 1) {
              firstRebuilt.set(rebuilt)
              failNextRead.set(true)
            } else if (call == 2) retryRebuilt.set(rebuilt)
          }
          result
        }

        override def readWallet(state: ErgoWalletState,
                                testMnemonic: Option[SecretString],
                                testKeysQty: Option[Int],
                                secretStorageSettings: SecretStorageSettings): ErgoWalletState = {
          if (failNextRead.compareAndSet(true, false)) {
            faultTriggered.set(true)
            throw new IOException("injected read after registry recreation")
          }
          super.readWallet(state, testMnemonic, testKeysQty, secretStorageSettings)
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, getHistory
      ) {
        override protected[wallet] def quarantinedWallet(state: ErgoWalletState,
                                                         reason: Throwable): Receive = {
          if (Option(reason.getCause).exists(
            _.getMessage == "injected read after registry recreation")) {
            failedActorState.set(state)
          }
          super.quarantinedWallet(state, reason)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, RescanWallet(first.height))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.rescanState shouldBe WalletRescanState.NeedsRecovery
          status.error.value should include("Wallet rescan recovery could not start")
          status.height shouldBe 0
        }
        markersBeforeRecreate.get() shouldBe true
        faultTriggered.get() shouldBe true
        firstRebuilt.get() should not be null
        failedActorState.get() should not be null
        firstRebuilt.get().registry.committedVersionAndDigest.get._2.height shouldBe 0
        (failedActorState.get().registry eq firstRebuilt.get().registry) shouldBe true

        probe.send(actor, RescanWallet(first.height))
        probe.expectMsg(5.seconds, Success(()))
        eventually(timeout(10.seconds), interval(100.millis)) {
          retryInput.get() should not be null
        }
        (retryInput.get().registry eq firstRebuilt.get().registry) shouldBe true
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.rescanState shouldBe WalletRescanState.Inactive
          status.error shouldBe None
        }
        recreateCalls.get() shouldBe 2
        retryRebuilt.get() should not be null
        retryRebuilt.get().registry.committedVersionAndDigest.get._1 shouldBe first.id
      } finally {
        val staleRegistry = Option(failedActorState.get()).exists { failed =>
          Option(firstRebuilt.get()).exists(rebuilt => failed.registry ne rebuilt.registry)
        }
        if (staleRegistry) {
          // A stale state cannot close the replacement registry, so release it explicitly.
          w.actorSystem.stop(actor)
          probe.expectTerminated(actor, 5.seconds)
          firstRebuilt.get().registry.close()
          firstRebuilt.get().storage.close()
        } else {
          probe.send(actor, CloseWallet)
          probe.expectTerminated(actor, 5.seconds)
        }
      }
      retryRebuilt.get().registry.committedVersionAndDigest.isFailure shouldBe true
    }
  }

  property("registry recreation rejects a silently retained old database") {
    withFixture { implicit w =>
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "retained-registry-after-delete-failure").getAbsolutePath
      )
      val state = ErgoWalletState.initial(actorSettings, parameters).get
      val registryFolder = WalletRegistry.registryFolder(actorSettings)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def deleteRecursive(root: File): Unit = ()
      }
      var reopened: Option[ErgoWalletState] = None
      try {
        registryFolder.isDirectory shouldBe true
        state.storage.beginRescanRecovery().get
        state.storage.quarantineDeepFork().get
        val result = service.recreateRegistry(state, actorSettings)
        reopened = result.toOption
        result.isFailure shouldBe true
        registryFolder.isDirectory shouldBe true
        state.storage.pendingRescanStartHeight.get shouldBe Some(1)
        state.storage.deepForkQuarantine.get shouldBe true
      } finally {
        reopened.foreach(_.registry.close())
        state.storage.close()
      }
    }
  }
}
