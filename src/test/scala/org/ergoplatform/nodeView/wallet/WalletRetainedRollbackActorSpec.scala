package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorSystem, Props}
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.core.VersionTag
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.history.ErgoHistoryReader.{FullChainCursor, FullChainProbe, FullChainSelected}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.{WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.{File, IOException}
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
        probe.send(actor, GetWalletStatus)
        val status = probe.expectMsgType[WalletStatus](5.seconds)
        status.height shouldBe second.height
        status.error shouldBe None

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

  property("failed intent clear fences a durably rolled-back registry") {
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
        probe.send(actor, GetWalletStatus)
        val status = probe.expectMsgType[WalletStatus](5.seconds)
        status.height shouldBe second.height
        status.error shouldBe None

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
      ErgoWalletState.initial(actorSettings, parameters).failed.get.getMessage should include(
        "Pending wallet retained rollback"
      )
    }
  }
}
