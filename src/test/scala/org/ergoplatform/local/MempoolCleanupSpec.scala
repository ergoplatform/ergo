package org.ergoplatform.local

import akka.actor.{ActorIdentity, ActorRef, ActorSystem, Identify, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.consensus.ProgressInfo
import org.ergoplatform.core.versionToId
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{InitStateFromSnapshot, RecheckMempool}
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, LocallyGeneratedModifier}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{EliminateTransactions, LocallyGeneratedTransaction, RecheckedTransactions}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.EmptyBlockSectionProcessor
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.mempool.ErgoMemPoolUtils.ProcessingOutcome
import org.ergoplatform.nodeView.state.{ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{MempoolTestHelpers, NodeViewTestOps}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scorex.crypto.authds.avltree.batch.PersistentBatchAVLProver
import scorex.crypto.hash.Digest32
import scorex.db.LDBVersionedStore
import scorex.util.ModifierId
import java.util.UUID
import java.util.concurrent.{CountDownLatch, TimeUnit}

class MempoolCleanupSpec extends AnyFlatSpec with Matchers with NodeViewTestOps with MempoolTestHelpers {
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  it should "terminate a cleanup whose pool read fails and accept the next cleanup" in {
    implicit val system: ActorSystem = ActorSystem()
    val probe = TestProbe()
    val (state, _) = createUtxoState(settings)
    val worker = system.actorOf(Props(new CleanupWorker(settings.nodeSettings)))
    val failedPool = new FakeMempool(Seq.empty) {
      override def getAllPrioritized: Seq[UnconfirmedTransaction] =
        throw new IllegalStateException("bounded pool read failure")
    }
    try {
      val failedJob = UUID.randomUUID()
      probe.send(worker, RunCleanup(failedJob, RecheckMempool(state, failedPool, UUID.randomUUID())))
      val failed = probe.expectMsgType[CleanupDone](3.seconds)
      failed.jobId shouldBe failedJob
      failed.result.failed.get.getMessage shouldBe "bounded pool read failure"
      val nextJob = UUID.randomUUID()
      probe.send(worker, RunCleanup(nextJob, RecheckMempool(state, new FakeMempool(Seq.empty), UUID.randomUUID())))
      val completed = probe.expectMsgType[CleanupDone](3.seconds)
      completed.jobId shouldBe nextJob
      completed.result.isSuccess shouldBe true
    } finally {
      Await.result(system.terminate(), 10.seconds)
      state.closeStorage()
    }
  }

  private def withEntry(test: (NodeViewFixture, RecheckMempool, UnconfirmedTransaction, ErgoFullBlock) => Unit): Unit = {
    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._
      val (state, boxes) = createUtxoState(fixture.settings)
      val requests = TestProbe()(actorSystem)
      actorSystem.eventStream.subscribe(requests.ref, classOf[RecheckMempool])
      try {
        val genesis = validFullBlock(None, state, boxes)
        applyBlock(genesis).isSuccess shouldBe true
        val request = requests.expectMsgType[RecheckMempool]
        val spendable = ErgoState.newBoxes(genesis.transactions).filter(_.ergoTree == TrueTree).toIndexedSeq.sortBy(-_.value)
        val tx = validTransactionFromBoxes(spendable.take(1))
        nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))
        expectMsgType[ProcessingOutcome.Accepted]
        val original = getCurrentView.pool.getAll.head
        test(fixture, request.copy(mempool = getCurrentView.pool), original, genesis)
      } finally {
        Await.result(getCurrentView.vault.getWalletStatus, 10.seconds)
        state.closeStorage()
      }
    }
  }

  private def checked(request: RecheckMempool, original: UnconfirmedTransaction, cost: Int = 1): RecheckedTransactions =
    RecheckedTransactions(request.stateRevision, request.mempool.getAll, Seq(original.withCost(cost)), Seq.empty)

  it should "leave an entry removed from the current pool absent after cleanup" in {
    withEntry { (fixture, request, original, _) =>
      import fixture._
        nodeViewHolderRef ! EliminateTransactions(Seq(original.id))
        getPoolSize shouldBe 0
        nodeViewHolderRef ! checked(request, original)
        getPoolSize shouldBe 0
    }
  }

  it should "refresh a present entry even after an unrelated admission" in {
    withEntry { (fixture, request, original, _) =>
      import fixture._
      val child = validTransactionFromBoxes(original.transaction.outputs.take(1))
      nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(child, None))
      expectMsgType[ProcessingOutcome.Accepted]
      nodeViewHolderRef ! checked(request, original)
      getCurrentView.pool.getAll(Seq(original.id)).head.lastCost shouldBe Some(1)
      getPoolSize shouldBe 2
    }
  }

  it should "retain a newer wrapper when an obsolete refresh or invalidation arrives" in {
    withEntry { (fixture, request, original, _) =>
      import fixture._
      nodeViewHolderRef ! checked(request, original, 2)
      getCurrentView.pool.getAll(Seq(original.id)).head.lastCost shouldBe Some(2)
      nodeViewHolderRef ! checked(request, original, 1)
      nodeViewHolderRef ! RecheckedTransactions(request.stateRevision, request.mempool.getAll, Seq.empty, Seq(original.id))
      getCurrentView.pool.getAll(Seq(original.id)).head.lastCost shouldBe Some(2)
    }
  }

  it should "invalidate a present eligible entry on a normal negative result" in {
    withEntry { (fixture, request, original, _) =>
      import fixture._
      nodeViewHolderRef ! RecheckedTransactions(request.stateRevision, request.mempool.getAll, Seq.empty, Seq(original.id))
      getPoolSize shouldBe 0
    }
  }

  it should "discard both outcomes after the owner applies a new state" in {
    withEntry { (fixture, request, original, genesis) =>
      import fixture._
      val unspent = ErgoState.newBoxes(genesis.transactions).filter { box =>
        box.ergoTree == TrueTree && !original.transaction.inputs.exists(_.boxId.sameElements(box.id))
      }.take(1).toIndexedSeq
      val blockTx = validTransactionFromBoxes(unspent)
      val block = validFullBlock(Some(genesis), getCurrentState.asInstanceOf[UtxoState], Seq(blockTx))
      applyBlock(block).isSuccess shouldBe true
      getCurrentView.pool.getAll(Seq(original.id)).head should be theSameInstanceAs original
      nodeViewHolderRef ! checked(request, original)
      nodeViewHolderRef ! RecheckedTransactions(request.stateRevision, request.mempool.getAll, Seq.empty, Seq(original.id))
      getCurrentView.pool.getAll(Seq(original.id)).head should be theSameInstanceAs original
    }
  }

  for (dataInput <- Seq(false, true)) {
    it should s"discard both outcomes when a ${if (dataInput) "data" else "spending"} input leaves the pool" in {
      withEntry { (fixture, request, parent, genesis) =>
        import fixture._
        val parentOutputs = parent.transaction.outputs
        val prerequisite = validTransactionFromBoxes(parentOutputs.take(1))
        nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(prerequisite, None))
        expectMsgType[ProcessingOutcome.Accepted]
        val target = if (dataInput) {
          val confirmedInputs = ErgoState.newBoxes(genesis.transactions).filter { box =>
            box.ergoTree == TrueTree && !parent.transaction.inputs.exists(_.boxId.sameElements(box.id))
          }.take(1).toIndexedSeq
          validTransactionFromBoxes(confirmedInputs, dataBoxes = prerequisite.outputs.take(1))
        } else {
          validTransactionFromBoxes(prerequisite.outputs.take(1))
        }
        nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(target, None))
        expectMsgType[ProcessingOutcome.Accepted]
        val snapshot = getCurrentView.pool
        val original = snapshot.getAll(Seq(target.id)).head
        val targetRequest = request.copy(mempool = snapshot)
        nodeViewHolderRef ! EliminateTransactions(Seq(prerequisite.id))
        getCurrentView.pool.getAll(Seq(target.id)).head should be theSameInstanceAs original
        nodeViewHolderRef ! checked(targetRequest, original)
        nodeViewHolderRef ! RecheckedTransactions(request.stateRevision, snapshot.getAll, Seq.empty, Seq(target.id))
        getCurrentView.pool.getAll(Seq(target.id)).head should be theSameInstanceAs original
      }
    }
  }

  it should "ignore obsolete completion and run the latest queued request after failure" in {
    implicit val system: ActorSystem = ActorSystem()
    val holder = TestProbe()
    val reads = TestProbe()
    val (state, _) = createUtxoState(settings)
    val release = new CountDownLatch(1)
    val failingPool = new FakeMempool(Seq.empty) {
      override def getAllPrioritized: Seq[UnconfirmedTransaction] = {
        reads.ref ! "first"
        require(release.await(10, TimeUnit.SECONDS), "bounded release timeout")
        throw new IllegalStateException("bounded queued failure")
      }
    }
    def pool(label: String): FakeMempool = new FakeMempool(Seq.empty) {
      override def getAllPrioritized: Seq[UnconfirmedTransaction] = {
        reads.ref ! label
        Seq.empty
      }
    }
    val auditor = system.actorOf(Props(new MempoolAuditor(holder.ref, holder.ref, settings)))
    try {
      holder.send(auditor, RecheckMempool(state, failingPool, UUID.randomUUID()))
      reads.expectMsg("first")
      system.actorSelection(auditor.path / "*").tell(Identify("worker"), holder.ref)
      val worker = holder.expectMsgType[ActorIdentity].ref.get
      val request = RecheckMempool(state, pool("latest"), UUID.randomUUID())
      holder.send(auditor, request.copy(mempool = pool("superseded")))
      holder.send(auditor, request)
      auditor.tell(CleanupDone(UUID.randomUUID(), Success(RecheckedTransactions(UUID.randomUUID(), Seq.empty, Seq.empty, Seq.empty))), worker)
      reads.expectNoMessage(100.millis)
      release.countDown()
      reads.expectMsg("latest")
      holder.expectMsgType[RecheckedTransactions].stateRevision shouldBe request.stateRevision
      holder.send(auditor, RecheckMempool(state, pool("next"), UUID.randomUUID()))
      reads.expectMsg("next")
      holder.expectMsgType[RecheckedTransactions]
    } finally {
      release.countDown()
      Await.result(system.terminate(), 10.seconds)
      state.closeStorage()
    }
  }

  it should "reject a wrong sender and a completed job replay while preserving queued work" in {
    implicit val system: ActorSystem = ActorSystem()
    val holder = TestProbe()
    val intercepted = TestProbe()
    val reads = TestProbe()
    val release = new CountDownLatch(1)
    val (state, _) = createUtxoState(settings)
    // Hold the actual terminal envelope, so the UUID is the real active job's
    // identity; only its sender is changed in the negative case.
    val auditor = TestActorRef(new MempoolAuditor(holder.ref, holder.ref, settings) {
      private var holdFirstCompletion = true
      override def aroundReceive(receive: Receive, message: Any): Unit = message match {
        case done: CleanupDone if holdFirstCompletion =>
          holdFirstCompletion = false
          intercepted.ref ! (done -> sender())
        case _ => super.aroundReceive(receive, message)
      }
    })
    try {
      holder.send(auditor, RecheckMempool(state, new FakeMempool(Seq.empty), UUID.randomUUID()))
      val (done, worker) = intercepted.expectMsgType[(CleanupDone, ActorRef)]
      val secondPool = new FakeMempool(Seq.empty) {
        override def getAllPrioritized: Seq[UnconfirmedTransaction] = {
          reads.ref ! "second"
          require(release.await(10, TimeUnit.SECONDS), "bounded second-job release timeout")
          Seq.empty
        }
      }
      val secondRequest = RecheckMempool(state, secondPool, UUID.randomUUID())
      holder.send(auditor, secondRequest)
      holder.send(auditor, done)
      holder.expectNoMessage(100.millis)
      reads.expectNoMessage(100.millis)
      auditor.tell(done, worker)
      holder.expectMsg(done.result.get)
      reads.expectMsg("second")
      val thirdPool = new FakeMempool(Seq.empty) {
        override def getAllPrioritized: Seq[UnconfirmedTransaction] = {
          reads.ref ! "third"
          Seq.empty
        }
      }
      val thirdRequest = RecheckMempool(state, thirdPool, UUID.randomUUID())
      holder.send(auditor, thirdRequest)
      auditor.tell(done, worker)
      holder.expectNoMessage(100.millis)
      reads.expectNoMessage(100.millis)
      release.countDown()
      holder.expectMsgType[RecheckedTransactions].stateRevision shouldBe secondRequest.stateRevision
      reads.expectMsg("third")
      holder.expectMsgType[RecheckedTransactions].stateRevision shouldBe thirdRequest.stateRevision
    } finally {
      release.countDown()
      Await.result(system.terminate(), 10.seconds)
      state.closeStorage()
    }
  }

  // These bounded seams script state outcomes and snapshot reconstruction.
  // The real holder owns installation, epoch changes and cleanup consumption.
  private class ScriptedState(base: UtxoState, nodeSettings: ErgoSettings)
    extends UtxoState(base.persistentProver, base.version, base.store, nodeSettings) {
    var rejectUpdate = false
    var attempts = 0
    override def applyModifier(mod: BlockSection, estimatedTip: Option[Int])
                              (generate: LocallyGeneratedModifier => Unit): Try[UtxoState] = {
      attempts += 1
      if (rejectUpdate) Failure(new IllegalStateException("bounded state update failure"))
      else Success(this)
    }
  }

  private class ScriptedHistory(section: BlockSection, state: UtxoState,
                                override protected val settings: ErgoSettings)
    extends ErgoHistory with EmptyBlockSectionProcessor {
    override val historyStorage: HistoryStorage = null
    override val powScheme = settings.chainSettings.powScheme
    var snapshotFails = false
    var snapshotAttempts = 0
    var snapshotInstalled = false
    var invalidReports = 0
    override def contains(id: ModifierId): Boolean = false
    override def estimatedTip(): Option[Int] = None
    override def headersHeight: Int = 1
    override def fullBlockHeight: Int = 1
    override def bestHeaderIdOpt: Option[ModifierId] = None
    override def closeStorage(): Unit = ()
    override def append(mod: BlockSection): Try[(ErgoHistory, ProgressInfo[BlockSection])] =
      Success(this -> ProgressInfo(None, Seq.empty, Seq(section), Seq.empty))
    override def reportModifierIsValid(mod: BlockSection): Try[ErgoHistory] = Success(this)
    override def reportModifierIsInvalid(mod: BlockSection, progress: ProgressInfo[BlockSection])
      : Try[(ErgoHistory, ProgressInfo[BlockSection])] = {
      invalidReports += 1
      Failure(new IllegalStateException("bounded state update report failure"))
    }
    override def isUtxoSnapshotApplied: Boolean = snapshotInstalled
    override def onUtxoSnapshotApplied(height: Int): Unit = snapshotInstalled = true
    override def createPersistentProver(store: LDBVersionedStore, history: ErgoHistoryReader,
                                        height: Int, blockId: ModifierId)
      : Try[PersistentBatchAVLProver[Digest32, Algos.HF]] = {
      snapshotAttempts += 1
      if (snapshotFails) Failure(new IllegalStateException("bounded snapshot reconstruction failure"))
      else Success(state.persistentProver)
    }
  }

  private class Holder(nodeSettings: ErgoSettings, initialView: (ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool))
    extends ErgoNodeViewHolder[UtxoState](nodeSettings) {
    override def restoreState(): Option[NodeView] = Some(initialView)
    def applySection(section: BlockSection): Unit = pmodModify(section, local = false)
    def pool: ErgoMemPool = memoryPool()
    def state: UtxoState = minimalState()
    def installPool(pool: ErgoMemPool): Unit = updateNodeView(updatedMempool = Some(pool))
  }

  private def withOwner(test: (TestActorRef[Holder], ScriptedState, ScriptedHistory,
                               RecheckMempool, ErgoFullBlock) => Unit): Unit = {
    implicit val system: ActorSystem = ActorSystem()
    val (base, boxes) = createUtxoState(settings)
    val genesis = validFullBlock(None, base, boxes)
    val updated = base.applyModifier(genesis, None)(_ => ()).get
    val state = new ScriptedState(updated, settings)
    val history = new ScriptedHistory(genesis.header, state, settings)
    val scans = TestProbe()
    val wallet = new ErgoWallet(history, settings, parameters) {
      override val walletActor: ActorRef = scans.ref
    }
    val requests = TestProbe()
    system.eventStream.subscribe(requests.ref, classOf[RecheckMempool])
    val holder = TestActorRef(new Holder(settings, (history, state, wallet, ErgoMemPool.empty(settings))))
    try {
      holder.underlyingActor.applySection(genesis.header)
      val request = requests.expectMsgType[RecheckMempool]
      test(holder, state, history, request, genesis)
    } finally Await.result(system.terminate(), 10.seconds)
  }

  private def installEntry(holder: TestActorRef[Holder], request: RecheckMempool,
                           genesis: ErgoFullBlock): (RecheckMempool, UnconfirmedTransaction) = {
    val boxes = ErgoState.newBoxes(genesis.transactions).filter(_.ergoTree == TrueTree).take(1).toIndexedSeq
    val entry = UnconfirmedTransaction(validTransactionFromBoxes(boxes), None)
    holder.underlyingActor.installPool(holder.underlyingActor.pool.put(entry))
    holder ! checked(request.copy(mempool = holder.underlyingActor.pool), entry, 3)
    val original = holder.underlyingActor.pool.getAll(Seq(entry.id)).head
    original.lastCost shouldBe Some(3)
    request.copy(mempool = holder.underlyingActor.pool) -> original
  }

  private def assertObsolete(holder: TestActorRef[Holder], request: RecheckMempool,
                             original: UnconfirmedTransaction): Unit = {
    holder ! checked(request, original)
    holder.underlyingActor.pool.getAll(Seq(original.id)).head should be theSameInstanceAs original
    holder ! RecheckedTransactions(request.stateRevision, request.mempool.getAll, Seq.empty, Seq(original.id))
    holder.underlyingActor.pool.getAll(Seq(original.id)).head should be theSameInstanceAs original
  }

  it should "discard the old epoch after a failed state attempt without installing a new state" in {
    withOwner { (holder, state, history, request, genesis) =>
      val (snapshot, original) = installEntry(holder, request, genesis)
      val installed = holder.underlyingActor.state
      state.rejectUpdate = true
      holder.underlyingActor.applySection(genesis.header)
      state.attempts shouldBe 2
      history.invalidReports shouldBe 1
      holder.underlyingActor.state should be theSameInstanceAs installed
      assertObsolete(holder, snapshot, original)
    }
  }

  for (fails <- Seq(false, true)) {
    it should s"discard the old epoch after ${if (fails) "failed" else "successful"} snapshot reconstruction" in {
      withOwner { (holder, state, history, request, genesis) =>
        val (snapshot, original) = installEntry(holder, request, genesis)
        history.snapshotFails = fails
        holder ! InitStateFromSnapshot(1, versionToId(state.version))
        history.snapshotAttempts shouldBe 1
        history.snapshotInstalled shouldBe !fails
        if (fails) holder.underlyingActor.state should be theSameInstanceAs state
        else holder.underlyingActor.state should not be theSameInstanceAs(state)
        assertObsolete(holder, snapshot, original)
      }
    }
  }

  for (dataInput <- Seq(false, true)) {
    it should s"discard both outcomes when a ${if (dataInput) "data" else "spending"} prerequisite enters the pool" in {
      withOwner { (holder, _, _, request, genesis) =>
        val boxes = ErgoState.newBoxes(genesis.transactions).filter(_.ergoTree == TrueTree).toIndexedSeq
        val prerequisite = validTransactionFromBoxes(boxes.take(1))
        val target = if (dataInput) {
          validTransactionFromBoxes(boxes.slice(1, 2), dataBoxes = prerequisite.outputs.take(1))
        } else validTransactionFromBoxes(prerequisite.outputs.take(1))
        val original = UnconfirmedTransaction(target, None)
        holder.underlyingActor.installPool(holder.underlyingActor.pool.put(original))
        val snapshot = request.copy(mempool = holder.underlyingActor.pool)
        holder.underlyingActor.installPool(holder.underlyingActor.pool.put(UnconfirmedTransaction(prerequisite, None)))
        holder.underlyingActor.pool.getAll(Seq(original.id)).head should be theSameInstanceAs original
        assertObsolete(holder, snapshot, original)
      }
    }
  }
}
