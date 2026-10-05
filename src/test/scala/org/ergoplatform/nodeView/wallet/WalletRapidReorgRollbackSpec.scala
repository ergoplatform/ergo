package org.ergoplatform.nodeView.wallet

import akka.actor.{Props, Status}
import akka.testkit.TestProbe
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{Rollback => HolderRollback}
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.persistence.WalletRegistry
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.generators.{ErgoNodeTransactionGenerators, ValidBlocksGenerators}
import org.ergoplatform.wallet.boxes.ChainStatus.OnChain
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.File
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.util.Try

class WalletRapidReorgRollbackSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {

  import org.ergoplatform.core.idToVersion
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  Seq(true, false).foreach { queuedBeforeFirstVerdict =>
    property(if (queuedBeforeFirstVerdict)
      "a queued selected rollback supersedes an off-chain rollback"
    else
      "a selected rollback arriving after the first verdict still avoids permanent quarantine") {
      withFixture { implicit w =>
      val address = getPublicKeys.head
      val first = makeGenesisBlock(address.pubkey)
      applyBlock(first) shouldBe 'success
      val initialBalance = eventually(timeout(10.seconds), interval(100.millis)) {
        val balance = getConfirmedBalances.walletBalance
        balance should be > 0L
        balance
      }
      def payment(amount: Long): PaymentRequest = PaymentRequest(
        Pay2SAddress(TrueTree)(w.settings.addressEncoder), amount, Array.empty, Map.empty
      )

      val secondATx = await(wallet.generateTransaction(Seq(payment(initialBalance / 2)))).get
      val secondCTx = await(wallet.generateTransaction(Seq(payment(initialBalance / 3)))).get
      val (cForkUtxo, cBoxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtFirstC = WrappedUtxoState(cForkUtxo, cBoxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
      val secondC = ValidBlocksGenerators.validFullBlock(Some(first), forkAtFirstC, Seq(secondCTx))
      val forkAtSecondC = forkAtFirstC.applyModifier(secondC)(_ => ()).get

      val secondA = makeNextBlock(getUtxoState, Seq(secondATx))
      applyBlock(secondA) shouldBe 'success
      val (bForkUtxo, bBoxHolder) = ValidBlocksGenerators.createUtxoState(w.settings)
      val forkAtSecondA = WrappedUtxoState(bForkUtxo, bBoxHolder, w.settings)
        .applyModifier(first)(_ => ()).get
        .applyModifier(secondA)(_ => ()).get
      val secondAExternal = secondA.blockTransactions.txs.flatMap(_.outputs)
        .find(_.ergoTree == TrueTree).get
      val thirdATx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(secondAExternal), stateCtxOpt = Some(getUtxoState.stateContext)
      )
      val thirdA = makeNextBlock(getUtxoState, Seq(thirdATx))
      applyBlock(thirdA) shouldBe 'success

      def nextForkBlock(parent: ErgoFullBlock, forkState: WrappedUtxoState): ErgoFullBlock = {
        val external = parent.blockTransactions.txs.flatMap(_.outputs)
          .find(_.ergoTree == TrueTree).get
        val tx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
          IndexedSeq(external), stateCtxOpt = Some(forkState.stateContext)
        )
        ValidBlocksGenerators.validFullBlock(Some(parent), forkState, Seq(tx))
      }
      val thirdBTx = ErgoNodeTransactionGenerators.validTransactionFromBoxes(
        IndexedSeq(secondAExternal), stateCtxOpt = Some(forkAtSecondA.stateContext)
      )
      val thirdB = ValidBlocksGenerators.validFullBlock(
        Some(secondA), forkAtSecondA, Seq(thirdBTx), Some(secondA.header.timestamp + 101)
      )
      thirdB.id should not equal thirdA.id
      val forkAtThirdB = forkAtSecondA.applyModifier(thirdB)(_ => ()).get
      val fourthB = nextForkBlock(thirdB, forkAtThirdB)

      val thirdC = nextForkBlock(secondC, forkAtSecondC)
      val forkAtThirdC = forkAtSecondC.applyModifier(thirdC)(_ => ()).get
      val fourthC = nextForkBlock(thirdC, forkAtThirdC)
      val forkAtFourthC = forkAtThirdC.applyModifier(fourthC)(_ => ()).get
      val fifthC = nextForkBlock(fourthC, forkAtFourthC)
      val expectedC = boxesAvailable(secondC, address.pubkey).map(_.value).sum

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir,
          if (queuedBeforeFirstVerdict) "wallet-rapid-reorg-queued" else "wallet-rapid-reorg-late"
        ).getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val seed = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, getHistory
      )))
      val seedProbe = TestProbe()(w.actorSystem)
      seedProbe.watch(seed)
      try {
        Seq(first, secondA, thirdA).foreach(block => seedProbe.send(seed, ScanOnChain(block)))
        seedProbe.send(seed, GetWalletStatus)
        val status = seedProbe.expectMsgType[WalletStatus](5.seconds)
        status.height shouldBe thirdA.height
        status.error shouldBe None
      } finally {
        seedProbe.send(seed, CloseWallet)
        seedProbe.expectTerminated(seed, 5.seconds)
      }
      val seededRegistry = WalletRegistry(actorSettings).get
      try {
        seededRegistry.hasVersion(idToVersion(first.id)) shouldBe true
        seededRegistry.hasVersion(idToVersion(secondA.id)) shouldBe true
        seededRegistry.committedVersionAndDigest.get._1 shouldBe thirdA.id
      } finally seededRegistry.close()

      val holdFirstProbe = new AtomicBoolean(true)
      val holdSecondProbe = new AtomicBoolean(true)
      val scanCalls = new AtomicInteger()
      val quarantineWrites = new AtomicInteger()
      val firstProbeSeen = TestProbe()(w.actorSystem)
      val firstProbeResolved = TestProbe()(w.actorSystem)
      val secondProbeSeen = TestProbe()(w.actorSystem)
      val history = getHistory
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(
          state: ErgoWalletState, block: ErgoFullBlock, dustLimit: Option[Long]
        ): Try[ErgoWalletState] = {
          scanCalls.incrementAndGet()
          super.scanBlockUpdate(state, block, dustLimit)
        }
      }
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, history
      ) {
        override protected def probeSelectedFullChain(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe = {
          if (targetId == secondA.id && holdFirstProbe.get()) {
            firstProbeSeen.ref ! targetId
            FullChainUnknown
          } else if (targetId == secondA.id) {
            val result = history.selectedFullChainProbe(targetId, targetHeight, cursor)
            firstProbeResolved.ref ! result
            result
          } else if (targetId == first.id && holdSecondProbe.get()) {
            secondProbeSeen.ref ! targetId
            FullChainUnknown
          } else {
            history.selectedFullChainProbe(targetId, targetHeight, cursor)
          }
        }

        override protected def persistDeepForkQuarantine(state: ErgoWalletState): Try[Unit] = {
          quarantineWrites.incrementAndGet()
          super.persistDeepForkQuarantine(state)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val actorProbe = TestProbe()(w.actorSystem)
      actorProbe.watch(actor)
      val holderProbe = TestProbe()(w.actorSystem)
      w.actorSystem.eventStream.subscribe(holderProbe.ref, classOf[HolderRollback])
      try {
        actorProbe.send(actor, GetWalletStatus)
        val initialStatus = actorProbe.expectMsgType[WalletStatus](5.seconds)
        initialStatus.height shouldBe thirdA.height
        initialStatus.error shouldBe None

        Seq(thirdB, fourthB).foreach(block => applyBlock(block) shouldBe 'success)
        eventually(timeout(10.seconds), interval(100.millis)) {
          getHistory.bestFullBlockIdOpt shouldBe Some(fourthB.id)
        }
        val firstRollback = holderProbe.expectMsgType[HolderRollback](5.seconds)
        firstRollback.branchPoint shouldBe secondA.id
        actorProbe.send(actor, Rollback(idToVersion(firstRollback.branchPoint)))
        firstProbeSeen.expectMsg(secondA.id)

        Seq(secondC, thirdC, fourthC, fifthC).foreach(block => applyBlock(block) shouldBe 'success)
        eventually(timeout(10.seconds), interval(100.millis)) {
          getHistory.bestFullBlockIdOpt shouldBe Some(fifthC.id)
          getCurrentState.version shouldBe idToVersion(fifthC.id)
        }
        val secondRollback = holderProbe.expectMsgType[HolderRollback](5.seconds)
        secondRollback.branchPoint shouldBe first.id
        getHistory.selectedFullChainProbe(secondA.id, secondA.height) shouldBe
          FullChainOther(fifthC.id)
        getHistory.selectedFullChainProbe(first.id, first.height) shouldBe
          FullChainSelected(fifthC.id)
        if (queuedBeforeFirstVerdict) {
          actorProbe.send(actor, Rollback(idToVersion(secondRollback.branchPoint)))
        }
        actorProbe.send(actor, GetWalletStatus)
        val queuedStatus = actorProbe.expectMsgType[WalletStatus](5.seconds)
        queuedStatus.height shouldBe thirdA.height
        queuedStatus.error.isDefined shouldBe true
        holdFirstProbe.set(false)

        if (!queuedBeforeFirstVerdict) {
          firstProbeResolved.expectMsg(FullChainOther(fifthC.id))
          actorProbe.send(actor, GetWalletStatus)
          val afterFirstVerdict = actorProbe.expectMsgType[WalletStatus](5.seconds)
          afterFirstVerdict.error.isDefined shouldBe true
          quarantineWrites.get() shouldBe 0
          actorProbe.send(actor, Rollback(idToVersion(secondRollback.branchPoint)))
        }

        secondProbeSeen.expectMsg(first.id)
        Seq(secondC, thirdC, fourthC, fifthC).foreach(block =>
          actorProbe.send(actor, ScanOnChain(block)))
        actorProbe.send(actor, ReadBalances(OnChain))
        actorProbe.expectMsgType[Status.Failure](5.seconds)
        actorProbe.send(actor, GetWalletBoxes(unspentOnly = true, considerUnconfirmed = false))
        actorProbe.expectMsgType[Status.Failure](5.seconds)
        actorProbe.send(actor, GetWalletStatus)
        val waitingStatus = actorProbe.expectMsgType[WalletStatus](5.seconds)
        waitingStatus.height shouldBe thirdA.height
        waitingStatus.error.isDefined shouldBe true
        scanCalls.get() shouldBe 0
        quarantineWrites.get() shouldBe 0

        holdSecondProbe.set(false)
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe fifthC.height
          status.error shouldBe None
          await(reader.confirmedBalances).walletBalance shouldBe expectedC
        }
        scanCalls.get() shouldBe 4
        quarantineWrites.get() shouldBe 0
      } finally {
        holdFirstProbe.set(false)
        holdSecondProbe.set(false)
        w.actorSystem.eventStream.unsubscribe(holderProbe.ref)
        actorProbe.send(actor, CloseWallet)
        actorProbe.expectTerminated(actor, 5.seconds)
      }

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.storage.deepForkQuarantine.get shouldBe false
        reopened.storage.retainedRollbackIntent.get shouldBe None
        val (tip, digest) = reopened.registry.committedVersionAndDigest.get
        tip shouldBe fifthC.id
        digest.height shouldBe fifthC.height
        digest.walletBalance shouldBe expectedC
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }
      }
    }
  }
}
