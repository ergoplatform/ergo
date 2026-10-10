package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorSystem, Props, Status}
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.core.VersionTag
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.history.ErgoHistoryReader.{FullChainCursor, FullChainOther, FullChainProbe, FullChainSelected, FullChainUnknown}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ChainStatus.OnChain
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.{File, IOException}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class WalletRetainedRollbackActorSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  import org.ergoplatform.core.idToVersion
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("retained rollback intent survives a stale applied-tip proof before clearing") {
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
        initialBalance / 2, Array.empty, Map.empty
      )
      val secondTx = await(wallet.generateTransaction(Seq(payment))).get
      val second = makeNextBlock(getUtxoState, Seq(secondTx))
      applyBlock(second) shouldBe 'success

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "retained-rollback-stale-clear").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val history = getHistory
      val observed = TestProbe()(w.actorSystem)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        private var selectedProofs = 0
        override protected def probeSelectedFullChain(targetId: ModifierId,
                                                      targetHeight: Int,
                                                      cursor: Option[FullChainCursor]): FullChainProbe = {
          val result = super.probeSelectedFullChain(targetId, targetHeight, cursor)
          if (targetId == first.id && result.isInstanceOf[FullChainSelected]) {
            selectedProofs += 1
            if (selectedProofs == 2) {
              history.recordHolderAppliedStateVersion(idToVersion(PreGenesisHeader.id))
              observed.ref ! selectedProofs
            }
          }
          result
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)
      try {
        Seq(first, second).foreach(block => probe.send(actor, ScanOnChain(block)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe second.height
          status.error shouldBe None
        }
        probe.send(actor, Rollback(idToVersion(first.id)))
        observed.expectMsg(2)
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds).error should not be None
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
        history.recordHolderAppliedStateVersion(idToVersion(second.id))
      }

      val storage = WalletStorage.readOrCreate(actorSettings)
      try storage.retainedRollbackIntent.get shouldBe Some(
        WalletStorage.RetainedRollbackIntent(second.id, first.id)
      ) finally storage.close()
    }
  }

  property("retained rollback reaches the selected ancestor and clears its intent") {
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
      getHistory.bestFullBlockIdOpt shouldBe Some(second.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "retained-rollback-actor").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        Seq(first, second).foreach(block => probe.send(actor, ScanOnChain(block)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe second.height
          status.error shouldBe None
          await(reader.confirmedBalances).walletBalance should be < initialBalance
        }

        probe.send(actor, Rollback(idToVersion(first.id)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          await(reader.confirmedBalances).walletBalance shouldBe initialBalance
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.storage.retainedRollbackIntent.get shouldBe None
        val (tip, digest) = reopened.registry.committedVersionAndDigest.get
        tip shouldBe first.id
        digest.height shouldBe first.height
        digest.walletBalance shouldBe initialBalance
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }
    }
  }

  property("failed retained rollback keeps its synced intent and fences restart") {
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
      getHistory.bestFullBlockIdOpt shouldBe Some(second.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "retained-rollback-fail-stop").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val stopOnShutdown = ConfigFactory.parseString(
        "akka.coordinated-shutdown.terminate-actor-system = on"
      ).withFallback(ConfigFactory.load())
      val failStopSystem = ActorSystem("wallet-retained-rollback-fail-stop", stopOnShutdown)

      try {
        val actor = failStopSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
        ) {
          override protected def rollbackRetainedRegistry(
            state: ErgoWalletState,
            version: VersionTag
          ): Try[Unit] = Failure(new IOException("injected durable registry failure"))
        }))
        val probe = TestProbe()(failStopSystem)
        probe.send(actor, ScanOnChain(first))
        probe.send(actor, ScanOnChain(second))
        eventually(timeout(10.seconds), interval(100.millis)) {
          probe.send(actor, GetWalletStatus)
          val status = probe.expectMsgType[WalletStatus](5.seconds)
          status.height shouldBe second.height
          status.error shouldBe None
        }

        probe.send(actor, Rollback(idToVersion(first.id)))
        Await.result(failStopSystem.whenTerminated, 20.seconds)
      } finally {
        if (!failStopSystem.whenTerminated.isCompleted) {
          Await.result(failStopSystem.terminate(), 20.seconds)
        }
      }

      val storage = WalletStorage.readOrCreate(actorSettings)
      try {
        storage.retainedRollbackIntent.get shouldBe Some(
          WalletStorage.RetainedRollbackIntent(second.id, first.id)
        )
      } finally storage.close()
      ErgoWalletState.initial(actorSettings, parameters).isFailure shouldBe true
    }
  }

  property("a completed retained rollback reopens only after selected applied-chain proof") {
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
      getHistory.bestFullBlockIdOpt shouldBe Some(second.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "retained-rollback-clear-failure").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val stopOnShutdown = ConfigFactory.parseString(
        "akka.coordinated-shutdown.terminate-actor-system = on"
      ).withFallback(ConfigFactory.load())
      val failStopSystem = ActorSystem("wallet-retained-rollback-clear-failure", stopOnShutdown)

      try {
        val actor = failStopSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
        ) {
          override protected def clearRetainedRollbackIntent(
            state: ErgoWalletState,
            source: ModifierId,
            target: ModifierId
          ): Try[Unit] = Failure(new IOException("injected intent clear failure"))
        }))
        val probe = TestProbe()(failStopSystem)
        probe.send(actor, ScanOnChain(first))
        probe.send(actor, ScanOnChain(second))
        eventually(timeout(10.seconds), interval(100.millis)) {
          probe.send(actor, GetWalletStatus)
          val status = probe.expectMsgType[WalletStatus](5.seconds)
          status.height shouldBe second.height
          status.error shouldBe None
        }

        probe.send(actor, Rollback(idToVersion(first.id)))
        Await.result(failStopSystem.whenTerminated, 20.seconds)
      } finally {
        if (!failStopSystem.whenTerminated.isCompleted) {
          Await.result(failStopSystem.terminate(), 20.seconds)
        }
      }

      val registry = WalletRegistry(actorSettings).get
      try {
        val (tip, digest) = registry.committedVersionAndDigest.get
        tip shouldBe first.id
        digest.height shouldBe first.height
      } finally registry.close()

      val storage = WalletStorage.readOrCreate(actorSettings)
      try {
        storage.retainedRollbackIntent.get shouldBe Some(
          WalletStorage.RetainedRollbackIntent(second.id, first.id)
        )
      } finally storage.close()

      val pending = ErgoWalletState.initial(actorSettings, parameters).get
      try pending.storage.retainedRollbackIntent.get shouldBe Some(
        WalletStorage.RetainedRollbackIntent(second.id, first.id)
      ) finally {
        pending.registry.close()
        pending.storage.close()
      }

      val syncFailureProbe = TestProbe()(w.actorSystem)
      val syncFailure = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      ) {
        override protected def syncRetainedRollbackCheckpoint(
          state: ErgoWalletState, target: ModifierId, height: Int
        ): Try[Unit] = Failure(new IOException("injected checkpoint sync failure"))
      }))
      syncFailureProbe.watch(syncFailure)
      try {
        syncFailureProbe.send(syncFailure, GetWalletStatus)
        syncFailureProbe.expectMsgType[WalletStatus](5.seconds).error should not be None
        syncFailureProbe.send(syncFailure, ReadBalances(OnChain))
        syncFailureProbe.expectMsgType[Status.Failure](5.seconds)
      } finally {
        syncFailureProbe.send(syncFailure, CloseWallet)
        syncFailureProbe.expectTerminated(syncFailure, 5.seconds)
      }
      val stillPendingAfterSyncFailure = WalletStorage.readOrCreate(actorSettings)
      try stillPendingAfterSyncFailure.retainedRollbackIntent.get shouldBe Some(
        WalletStorage.RetainedRollbackIntent(second.id, first.id)
      ) finally stillPendingAfterSyncFailure.close()

      val history = getHistory
      history.recordHolderAppliedStateVersion(idToVersion(PreGenesisHeader.id))
      try {
        val waiting = w.actorSystem.actorOf(Props(new ErgoWalletActor(
          actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
        )))
        val waitingProbe = TestProbe()(w.actorSystem)
        waitingProbe.watch(waiting)
        try {
          waitingProbe.send(waiting, GetWalletStatus)
          val status = waitingProbe.expectMsgType[WalletStatus](5.seconds)
          status.height shouldBe second.height
          status.error should not be None
          waitingProbe.send(waiting, ReadBalances(OnChain))
          waitingProbe.expectMsgType[Status.Failure](5.seconds)
        } finally {
          waitingProbe.send(waiting, CloseWallet)
          waitingProbe.expectTerminated(waiting, 5.seconds)
        }
      } finally {
        history.recordHolderAppliedStateVersion(idToVersion(second.id))
      }

      val stillPending = WalletStorage.readOrCreate(actorSettings)
      try stillPending.retainedRollbackIntent.get shouldBe Some(
        WalletStorage.RetainedRollbackIntent(second.id, first.id)
      ) finally stillPending.close()

      val recovered = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      val recoveredReader = new ErgoWalletReader { override val walletActor = recovered }
      val recoveredProbe = TestProbe()(w.actorSystem)
      recoveredProbe.watch(recovered)
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(recoveredReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          await(recoveredReader.confirmedBalances).walletBalance shouldBe initialBalance
        }
      } finally {
        recoveredProbe.send(recovered, CloseWallet)
        recoveredProbe.expectTerminated(recovered, 5.seconds)
      }

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.storage.retainedRollbackIntent.get shouldBe None
        val (tip, digest) = reopened.registry.committedVersionAndDigest.get
        tip shouldBe first.id
        digest.height shouldBe first.height
        digest.walletBalance shouldBe initialBalance
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }

      val stagedForOther = WalletStorage.readOrCreate(actorSettings)
      try stagedForOther.beginRetainedRollback(second.id, first.id).get
      finally stagedForOther.close()

      val otherProbe = TestProbe()(w.actorSystem)
      val otherVerdict = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        override protected def probeSelectedFullChain(targetId: ModifierId,
                                                      targetHeight: Int,
                                                      cursor: Option[FullChainCursor]): FullChainProbe =
          if (targetId == first.id) {
            otherProbe.ref ! FullChainOther(second.id)
            FullChainOther(second.id)
          } else super.probeSelectedFullChain(targetId, targetHeight, cursor)
      }))
      otherProbe.watch(otherVerdict)
      try {
        otherProbe.expectMsg(FullChainOther(second.id))
        otherProbe.expectMsg(FullChainOther(second.id))
        otherProbe.send(otherVerdict, GetWalletStatus)
        otherProbe.expectMsgType[WalletStatus](5.seconds).error should not be None
        otherProbe.send(otherVerdict, ReadBalances(OnChain))
        otherProbe.expectMsgType[Status.Failure](5.seconds)
      } finally {
        otherProbe.send(otherVerdict, CloseWallet)
        otherProbe.expectTerminated(otherVerdict, 5.seconds)
      }

      val clearedAfterOther = WalletStorage.readOrCreate(actorSettings)
      try {
        clearedAfterOther.retainedRollbackIntent.get shouldBe None
        clearedAfterOther.deepForkQuarantine.get shouldBe true
        clearedAfterOther.clearDeepForkQuarantine().get
      } finally clearedAfterOther.close()

      val stagedAgain = WalletStorage.readOrCreate(actorSettings)
      try stagedAgain.beginRetainedRollback(second.id, first.id).get
      finally stagedAgain.close()

      val supersedingProbe = TestProbe()(w.actorSystem)
      val targetActivations = new AtomicInteger()
      val superseded = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        private var delayFirstProof = true
        override protected def probeSelectedFullChain(targetId: ModifierId,
                                                      targetHeight: Int,
                                                      cursor: Option[FullChainCursor]): FullChainProbe = {
          if (targetId == first.id && delayFirstProof) {
            delayFirstProof = false
            supersedingProbe.ref ! FullChainUnknown
            FullChainUnknown
          } else super.probeSelectedFullChain(targetId, targetHeight, cursor)
        }
        override protected[wallet] def activateWallet(newState: ErgoWalletState): Unit = {
          if (newState.getWalletHeight == first.height) targetActivations.incrementAndGet()
          super.activateWallet(newState)
        }
      }))
      val supersededReader = new ErgoWalletReader { override val walletActor = superseded }
      supersedingProbe.watch(superseded)
      try {
        supersedingProbe.expectMsg(FullChainUnknown)
        supersedingProbe.send(superseded, Rollback(idToVersion(PreGenesisHeader.id)))
        supersedingProbe.send(superseded, GetWalletStatus)
        supersedingProbe.expectMsgType[WalletStatus](5.seconds).error should not be None
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(supersededReader.getWalletStatus)
          status.height shouldBe 0
          status.error shouldBe None
          await(supersededReader.confirmedBalances).walletBalance shouldBe 0L
        }
      } finally {
        supersedingProbe.send(superseded, CloseWallet)
        supersedingProbe.expectTerminated(superseded, 5.seconds)
      }
      targetActivations.get() shouldBe 0

      val supersededState = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        supersededState.storage.retainedRollbackIntent.get shouldBe None
        val (tip, digest) = supersededState.registry.committedVersionAndDigest.get
        tip shouldBe PreGenesisHeader.id
        digest.height shouldBe 0
      } finally {
        supersededState.registry.close()
        supersededState.storage.close()
      }

      val sameVersionStorage = WalletStorage.readOrCreate(actorSettings)
      try sameVersionStorage.beginRetainedRollback(
        PreGenesisHeader.id, PreGenesisHeader.id).get
      finally sameVersionStorage.close()
      ErgoWalletState.initial(actorSettings, parameters).isFailure shouldBe true
    }
  }
}
