package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.consensus.ModifierSemanticValidity
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.history.ErgoHistoryReader.{FullChainCursor, FullChainOther, FullChainProbe, FullChainSelected, FullChainUnknown}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.{FalseTree, TrueTree}
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.generators.{ErgoNodeTransactionGenerators, ValidBlocksGenerators}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId
import sigma.ast.ErgoTree

import java.io.File
import scala.concurrent.duration._

class WalletProvisionalFullTipSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.core.idToVersion
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("a superseded off-chain rollback must not durably quarantine an applied wallet checkpoint") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val balance = eventually(timeout(10.seconds), interval(100.millis)) {
        val current = getConfirmedBalances.walletBalance
        current should be > 0L
        current
      }
      def payment(amount: Long): PaymentRequest =
        PaymentRequest(Pay2SAddress(TrueTree)(w.settings.addressEncoder), amount,
          Array.empty, Map.empty)
      val secondATx = await(wallet.generateTransaction(Seq(payment(balance / 2)))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondATx))
      val secondBTx = await(wallet.generateTransaction(Seq(payment(balance / 3)))).get
      val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirst = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondB = ValidBlocksGenerators.validFullBlock(Some(first), forkAtFirst, Seq(secondBTx))
      secondB.id should not equal secondA.id
      applyBlock(secondA) shouldBe 'success

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "superseded-missing-rollback").getAbsolutePath
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
        Seq(first, secondA).foreach(block => probe.send(actor, ScanOnChain(block)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe secondA.height
          status.error shouldBe None
        }
        getHistory.appliedFullChainProbe(secondA.id, secondA.height) shouldBe
          FullChainSelected(secondA.id)

        getHistory.append(secondB.header).get
        getHistory.heightOf(secondB.id) shouldBe Some(secondB.height)
        getHistory.bestFullBlockIdOpt shouldBe Some(secondA.id)

        // The holder has already moved back onto this wallet checkpoint when
        // an older, unretained rollback notification reaches the actor.
        probe.send(actor, Rollback(idToVersion(secondB.id)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe secondA.height
          status.error shouldBe None
        }
      } finally {
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val inspected = ErgoWalletState.initial(actorSettings, parameters).get
      try inspected.storage.deepForkQuarantine.get shouldBe false
      finally {
        inspected.registry.close()
        inspected.storage.close()
      }
    }
  }

  property("startup does not activate or clear a marker after its applied-tip proof becomes stale") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "stale-applied-tip-marker-clear").getAbsolutePath
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val history = getHistory
      def openActor() = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      )))
      def openInterleaved(observed: TestProbe) = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        private var injectOnce = true
        private def observe(result: FullChainProbe): FullChainProbe = {
          if (injectOnce && result == FullChainSelected(first.id)) {
            injectOnce = false
            history.recordHolderAppliedStateVersion(idToVersion(PreGenesisHeader.id))
            observed.ref ! result
          }
          result
        }
        override protected def probeSelectedFullChain(targetId: ModifierId,
                                                      targetHeight: Int,
                                                      cursor: Option[FullChainCursor]): FullChainProbe = {
          observe(super.probeSelectedFullChain(targetId, targetHeight, cursor))
        }
        override protected def probeSelectedFullChainBodies(targetId: ModifierId,
                                                            targetHeight: Int,
                                                            cursor: Option[FullChainCursor]): FullChainProbe =
          observe(super.probeSelectedFullChainBodies(targetId, targetHeight, cursor))
      }))
      def closeActor(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val seed = openActor()
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      try {
        seed ! ScanOnChain(first)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(seedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally closeActor(seed)

      val activationObserved = TestProbe()(w.actorSystem)
      val waiting = openInterleaved(activationObserved)
      try {
        activationObserved.expectMsg(FullChainSelected(first.id))
        val statusProbe = TestProbe()(w.actorSystem)
        statusProbe.send(waiting, GetWalletStatus)
        statusProbe.expectMsgType[WalletStatus](5.seconds).error should not be None
      } finally closeActor(waiting)
      history.recordHolderAppliedStateVersion(idToVersion(first.id))

      val marked = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        marked.storage.deepForkQuarantine.get shouldBe false
        marked.storage.quarantineDeepFork().get
      }
      finally {
        marked.registry.close()
        marked.storage.close()
      }

      val observed = TestProbe()(w.actorSystem)
      val interleaved = openInterleaved(observed)
      try {
        observed.expectMsg(FullChainSelected(first.id))
        val statusProbe = TestProbe()(w.actorSystem)
        statusProbe.send(interleaved, GetWalletStatus)
        statusProbe.expectMsgType[WalletStatus](5.seconds).error should not be None
      } finally closeActor(interleaved)

      val stillMarked = ErgoWalletState.initial(actorSettings, parameters).get
      try stillMarked.storage.deepForkQuarantine.get shouldBe true
      finally {
        stillMarked.registry.close()
        stillMarked.storage.close()
      }

      history.recordHolderAppliedStateVersion(idToVersion(first.id))
      val recovered = openActor()
      val recoveredReader = new ErgoWalletReader { override val walletActor = recovered }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(recoveredReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally closeActor(recovered)
    }
  }

  property("a semantically rejected provisional full tip does not durably quarantine the wallet") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }

      def payment(tree: ErgoTree, amount: Long): PaymentRequest =
        PaymentRequest(Pay2SAddress(tree)(w.settings.addressEncoder), amount, Array.empty, Map.empty)

      // The wallet commits A2 through the holder. B2 spends the same A1 output on a fork.
      val secondATx = await(wallet.generateTransaction(Seq(payment(TrueTree, initialBalance / 2)))).get
      val secondA = makeNextBlock(getUtxoState, Seq(secondATx))
      val secondBTx = await(wallet.generateTransaction(Seq(
        payment(FalseTree, initialBalance / 3), payment(TrueTree, initialBalance / 4)
      ))).get
      val (forkUtxo, boxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirst = WrappedUtxoState(forkUtxo, boxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondB = ValidBlocksGenerators.validFullBlock(Some(first), forkAtFirst, Seq(secondBTx))
      val forkAtSecondB = forkAtFirst.applyModifier(secondB)(_ => ()).get
      secondA.id should not equal secondB.id
      applyBlock(secondA) shouldBe 'success

      val falseTreeBox = secondBTx.outputs.find(_.ergoTree == FalseTree).get
      val invalidTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(falseTreeBox), stateCtxOpt = Some(forkAtSecondB.stateContext)
      )
      val thirdB = ValidBlocksGenerators.validFullBlock(Some(secondB), forkAtSecondB, Seq(invalidTx))
      val externalA = secondATx.outputs.find(_.ergoTree == TrueTree).get
      val thirdATx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(externalA), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val thirdA = makeNextBlock(getUtxoState, Seq(thirdATx))

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "provisional-full-tip-wallet").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def openActor() = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seed = openActor()
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        Seq(first, secondA).foreach(block => seedProbe.send(seed, ScanOnChain(block)))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(seedReader.getWalletStatus)
          status.height shouldBe secondA.height
          status.error shouldBe None
        }
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }

      val history = getHistory
      secondB.toSeq.foreach(section => history.append(section).get)
      val provisionalProgress = thirdB.toSeq.map(section => history.append(section).get._2)
        .find(_.toApply.nonEmpty).get
      history.bestFullBlockIdOpt shouldBe Some(thirdB.id)
      history.selectedFullChainProbe(secondA.id, secondA.height) shouldBe FullChainOther(thirdB.id)
      history.isSemanticallyValid(secondA.blockTransactions.id) shouldBe ModifierSemanticValidity.Valid
      history.isSemanticallyValid(thirdB.blockTransactions.id) shouldBe ModifierSemanticValidity.Unknown
      getCurrentState.version shouldBe idToVersion(secondA.id)
      forkAtSecondB.applyModifier(history.bestFullBlockOpt.get)(_ => ()).isFailure shouldBe true

      // Actor startup sees B3 after history selection but before state validation.
      val provisional = openActor()
      val provisionalReader = new ErgoWalletReader { override val walletActor = provisional }
      val provisionalProbe = TestProbe()(w.actorSystem)
      provisionalProbe.watch(provisional)
      try {
        await(provisionalReader.getWalletStatus).height shouldBe secondA.height
      } finally {
        provisionalProbe.send(provisional, CloseWallet)
        provisionalProbe.expectTerminated(provisional, 5.seconds)
      }

      // Invalidation searches below B3's parent and therefore selects B2 first.
      history.reportModifierIsInvalid(thirdB, provisionalProgress).get
      history.bestFullBlockIdOpt shouldBe Some(secondB.id)
      // A validity marker survives an earlier application of B2. It says that
      // B2 was valid once, but cannot say that the holder has reapplied it now.
      forkAtSecondB.version shouldBe idToVersion(secondB.id)
      history.reportModifierIsValid(secondB).get
      history.isSemanticallyValid(secondB.blockTransactions.id) shouldBe ModifierSemanticValidity.Valid
      getCurrentState.version shouldBe idToVersion(secondA.id)
      // A prior Valid marker does not mean the holder has reapplied B2 now.
      // The actor must hold the wallet closed without a permanent marker.
      history.appliedFullChainProbe(secondA.id, secondA.height) shouldBe FullChainUnknown
      val historicallyValid = openActor()
      val historicallyValidReader = new ErgoWalletReader {
        override val walletActor = historicallyValid
      }
      val historicallyValidProbe = TestProbe()(w.actorSystem)
      historicallyValidProbe.watch(historicallyValid)
      try {
        await(historicallyValidReader.getWalletStatus).error should not be None
      } finally {
        historicallyValidProbe.send(historicallyValid, CloseWallet)
        historicallyValidProbe.expectTerminated(historicallyValid, 5.seconds)
      }
      val heldState = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        heldState.storage.deepForkQuarantine.get shouldBe false
      } finally {
        heldState.registry.close()
        heldState.storage.close()
      }
      // A3 is built on the applied A2 state and overtakes the rejected fork's B2 tip.
      thirdA.toSeq.foreach(section => history.append(section).get)
      history.bestFullBlockIdOpt shouldBe Some(thirdA.id)
      getCurrentState.version shouldBe idToVersion(secondA.id)
      history.selectedFullChainProbe(secondA.id, secondA.height) shouldBe
        FullChainSelected(thirdA.id)

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.registry.committedVersionAndDigest.get._1 shouldBe secondA.id
        reopened.storage.deepForkQuarantine.get shouldBe false
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }

      // Continue the same history through a real holder reorganization. The
      // old B2 validity marker must not become authority until the holder has
      // rolled back A2 and installed the selected B branch.
      val transitioning = openActor()
      val transitioningReader = new ErgoWalletReader {
        override val walletActor = transitioning
      }
      val transitioningProbe = TestProbe()(w.actorSystem)
      transitioningProbe.watch(transitioning)
      try {
        await(transitioningReader.getWalletStatus).error should not be None
        getCurrentState.version shouldBe idToVersion(secondA.id)
        history.appliedFullChainProbe(secondA.id, secondA.height) shouldBe FullChainUnknown

        val externalB = secondBTx.outputs.find(_.ergoTree == TrueTree).get
        val validThirdBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(externalB), stateCtxOpt = Some(forkAtSecondB.stateContext)
        )
        val validThirdB = ValidBlocksGenerators.validFullBlock(
          Some(secondB), forkAtSecondB, Seq(validThirdBTx)
        )
        validThirdB.id should not equal thirdB.id
        val forkAtValidThirdB = forkAtSecondB.applyModifier(validThirdB)(_ => ()).get
        val externalB3 = validThirdBTx.outputs.find(_.ergoTree == TrueTree).get
        val validFourthBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(externalB3), stateCtxOpt = Some(forkAtValidThirdB.stateContext)
        )
        val validFourthB = ValidBlocksGenerators.validFullBlock(
          Some(validThirdB), forkAtValidThirdB, Seq(validFourthBTx)
        )
        val forkAtValidFourthB = forkAtValidThirdB.applyModifier(validFourthB)(_ => ()).get
        applyBlock(validThirdB) shouldBe 'success
        applyBlock(validFourthB) shouldBe 'success

        eventually(timeout(10.seconds), interval(100.millis)) {
          getCurrentState.version shouldBe idToVersion(validFourthB.id)
          getCurrentState.rootDigest.sameElements(forkAtValidFourthB.rootDigest) shouldBe true
          history.appliedFullChainProbe(secondA.id, secondA.height) shouldBe
            FullChainOther(validFourthB.id)
        }
        eventually(timeout(10.seconds), interval(100.millis)) {
          await(transitioningReader.getWalletStatus).error.value.toLowerCase should include("quarantine")
        }
      } finally {
        transitioningProbe.send(transitioning, CloseWallet)
        transitioningProbe.expectTerminated(transitioning, 5.seconds)
      }
      val afterAppliedFork = ErgoWalletState.initial(actorSettings, parameters).get
      try afterAppliedFork.storage.deepForkQuarantine.get shouldBe true
      finally {
        afterAppliedFork.registry.close()
        afterAppliedFork.storage.close()
      }
    }
  }

  property("a durable fork marker is cleared after the holder returns to the wallet checkpoint") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "recovered-fork-marker-wallet").getAbsolutePath
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      def openActor() = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      def closeActor(actor: akka.actor.ActorRef): Unit = {
        val probe = TestProbe()(w.actorSystem)
        probe.watch(actor)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val seed = openActor()
      val seedReader = new ErgoWalletReader { override val walletActor = seed }
      try {
        seed ! ScanOnChain(first)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(seedReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally closeActor(seed)

      val marked = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        marked.storage.quarantineDeepFork().get
        marked.storage.deepForkQuarantine.get shouldBe true
      } finally {
        marked.registry.close()
        marked.storage.close()
      }

      val recovered = openActor()
      val recoveredReader = new ErgoWalletReader { override val walletActor = recovered }
      try {
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(recoveredReader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
        }
      } finally closeActor(recovered)

      val inspected = ErgoWalletState.initial(actorSettings, parameters).get
      try inspected.storage.deepForkQuarantine.get shouldBe false
      finally {
        inspected.registry.close()
        inspected.storage.close()
      }
    }
  }
}
