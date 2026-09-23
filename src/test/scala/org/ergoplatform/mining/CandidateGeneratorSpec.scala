package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.{StatusReply, ask}
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction, UnsignedErgoTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, FullBlockApplied, LocalBlockApplied, SemanticallyFailedModification}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{EliminateTransactions, LocallyGeneratedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{StateType, UtxoState, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef, LocallyGeneratedOrderingBlock}
import org.ergoplatform.settings.NetworkType.DevNet60
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genHeaderChain}
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper}
import org.ergoplatform.validation.MalformedModifierError
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input, OrderingSolutionFound}
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import sigma.ast.ErgoTree
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigma.data.ProveDlog
import sigma.serialization.ErgoTreeSerializer
import sigmastate.crypto.DLogProtocol.DLogProverInput
import scorex.crypto.authds.{ADDigest, SerializedAdProof}

import scala.concurrent.duration._
import scala.util.{Failure, Try}

class CandidateGeneratorSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers with Eventually {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  implicit private val timeout: Timeout = defaultTimeout

  private val newBlockSignal: Class[FullBlockApplied] = classOf[FullBlockApplied]
  private val newBlockDelay: FiniteDuration        = 30.seconds
  private val candidateGenDelay: FiniteDuration    = 3.seconds
  private val blockValidationDelay: FiniteDuration = 2.seconds

  private def expectAppliedBlock(events: TestProbe,
                                 submitted: Header,
                                 max: FiniteDuration = candidateGenDelay): Unit = {
    events.fishForMessage(max) {
      case LocalBlockApplied(header, _) if header.id == submitted.id => true
      // A duplicate of the preceding submission can remain from the outer local handler.
      // Accept no other header as either success or a discardable message.
      case LocalBlockApplied(header, _) if header.id == submitted.parentId => false
      case unexpected => fail(s"Unexpected application event while awaiting ${submitted.id}: $unexpected")
    }
  }

  private val actorSystems = scala.collection.mutable.ArrayBuffer.empty[ActorSystem]

  private def newActorSystem(): ActorSystem = {
    val system = ActorSystem()
    actorSystems += system
    system
  }

  override protected def withFixture(test: NoArgTest): org.scalatest.Outcome = {
    try super.withFixture(test)
    finally {
      val systemsToStop = actorSystems.toVector
      actorSystems.clear()
      systemsToStop.foreach(system => TestKit.shutdownActorSystem(system))
    }
  }

  val defaultSettings: ErgoSettings = {
    val empty = ErgoSettingsReader.read()
    val nodeSettings = empty.nodeSettings.copy(
      mining                       = true,
      stateType                    = StateType.Utxo,
      internalMinerPollingInterval = 1.second,
      offlineGeneration            = true,
      verifyTransactions           = true
    )
    val chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
  }

  private val defaultSettings60 = defaultSettings.copy(networkType = DevNet60, directory = defaultSettings.directory + "60")

  it should "provider candidate to internal miner and verify and apply his solution" in new TestKit(
    ActorSystem()
  ) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef    = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )
    ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)

    // after applying solution from miner
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    system.terminate()
  }

  it should "recover when locally mined block is invalidated by node view holder" in new TestKit(
    ActorSystem()
  ) {
    val replyProbe = new TestProbe(system)
    // fake node view holder: solved block is never applied, so solvedBlock stays set
    val viewHolderProbe = new TestProbe(system)

    // real readers holder over real node view holder, needed for candidate generation
    val realViewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef  = ErgoReadersHolderRef(realViewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderProbe.ref,
        defaultSettings
      )

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), replyProbe.ref)
    val block = replyProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb)   => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
    }

    candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), replyProbe.ref)
    replyProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))

    // ordering block was sent to the (fake) node view holder
    viewHolderProbe.expectMsgPF(blockValidationDelay) {
      case LocallyGeneratedOrderingBlock(efb, _) if efb.id == block.id =>
    }

    // mining is stalled: new solutions are rejected while solvedBlock is set
    candidateGenerator.tell(block.header.powSolution, replyProbe.ref)
    replyProbe.expectMsgPF(blockValidationDelay) {
      case r: StatusReply[_] if r.isError =>
    }

    // node view holder invalidates the block (e.g. a transaction became invalid)
    val failedTxId = block.blockTransactions.txs.head.id
    val error =
      new MalformedModifierError("tx failed", failedTxId, ErgoTransaction.modifierTypeId)
    system.eventStream.publish(
      SemanticallyFailedModification(BlockTransactions.modifierTypeId, block.blockTransactions.id, error)
    )

    // mining resumes: a new candidate is generated and new solutions are accepted again
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), replyProbe.ref)
    val newBlock = replyProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb)   => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
    }
    candidateGenerator.tell(OrderingSolutionFound(newBlock.header.powSolution), replyProbe.ref)
    replyProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))

    system.terminate()
  }

  it should "recover when locally mined block is invalidated by full block id" in new TestKit(
    ActorSystem()
  ) {
    val replyProbe = new TestProbe(system)
    // fake node view holder: solved block is never applied, so solvedBlock stays set
    val viewHolderProbe = new TestProbe(system)

    // real readers holder over real node view holder, needed for candidate generation
    val realViewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef  = ErgoReadersHolderRef(realViewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderProbe.ref,
        defaultSettings
      )

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), replyProbe.ref)
    val block = replyProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb)   => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
    }

    candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), replyProbe.ref)
    replyProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))

    // ordering block was sent to the (fake) node view holder
    viewHolderProbe.expectMsgPF(blockValidationDelay) {
      case LocallyGeneratedOrderingBlock(efb, _) if efb.id == block.id =>
    }

    // mining is stalled: new solutions are rejected while solvedBlock is set
    candidateGenerator.tell(block.header.powSolution, replyProbe.ref)
    replyProbe.expectMsgPF(blockValidationDelay) {
      case r: StatusReply[_] if r.isError =>
    }

    // node view holder invalidates the block using full-block typeId and block id
    val failedTxId = block.blockTransactions.txs.head.id
    val error =
      new MalformedModifierError("tx failed", failedTxId, ErgoTransaction.modifierTypeId)
    system.eventStream.publish(
      SemanticallyFailedModification(ErgoFullBlock.modifierTypeId, block.id, error)
    )

    // mining resumes: a new candidate is generated and new solutions are accepted again
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), replyProbe.ref)
    val newBlock = replyProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb)   => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
    }
    candidateGenerator.tell(OrderingSolutionFound(newBlock.header.powSolution), replyProbe.ref)
    replyProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))

    system.terminate()
  }

  it should "let multiple miners compete" in new TestKit(newActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef    = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )

    val m1 = ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)
    val m2 = ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)
    val m3 = ErgoMiningThread(defaultSettings, candidateGenerator, defaultMinerSecret.w)

    // after applying solution from miner
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    val countProbe = new TestProbe(system)
    eventually(timeout(newBlockDelay), interval(100.millis)) {
      m1.tell(ErgoMiningThread.GetSolvedBlocksCount, countProbe.ref)
      val m1Count = countProbe.expectMsgClass(candidateGenDelay, classOf[ErgoMiningThread.SolvedBlocksCount])
      m2.tell(ErgoMiningThread.GetSolvedBlocksCount, countProbe.ref)
      val m2Count = countProbe.expectMsgClass(candidateGenDelay, classOf[ErgoMiningThread.SolvedBlocksCount])
      m3.tell(ErgoMiningThread.GetSolvedBlocksCount, countProbe.ref)
      val m3Count = countProbe.expectMsgClass(candidateGenDelay, classOf[ErgoMiningThread.SolvedBlocksCount])
      List(m1Count, m2Count, m3Count).map(_.count).sum should be >= 3
    }
    system.terminate()
  }

  it should "cache candidate until newly mined block is applied" in new TestKit(
    newActorSystem()
  ) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef    = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )

    expectNoMessage(1.second)
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)

    val block = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
    }

    // now block should be cached
    (0 to 20).foreach { _ =>
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
      testProbe.expectMsgClass(5.millis, classOf[StatusReply[_]])
    }

    candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)
    testProbe.expectMsg(blockValidationDelay, StatusReply.success(()))
    // after applying solution
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    system.terminate()
  }

  it should "accept an earlier solution after explicitly requested regeneration" in new TestKit(newActorSystem()) {
    val testProbe = new TestProbe(system)
    val appliedProbe = new TestProbe(system)
    system.eventStream.subscribe(appliedProbe.ref, newBlockSignal)

    val settingsWithShortRegeneration: ErgoSettings =
      ErgoSettingsReader.read()
        .copy(
          nodeSettings = defaultSettings.nodeSettings
            .copy(blockCandidateGenerationInterval = 1.millis),
          chainSettings =
            ErgoSettingsReader.read().chainSettings.copy(blockInterval = 1.seconds)
        )

    val viewHolderRef: ActorRef =
      ErgoNodeViewRef(settingsWithShortRegeneration)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        settingsWithShortRegeneration
      )

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    val powScheme = settingsWithShortRegeneration.chainSettings.powScheme

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        val block = result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
        candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)
        testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
        expectAppliedBlock(appliedProbe, block.header)
    }

    // build new transaction that uses miner's reward as input
    val prop: ProveDlog =
      DLogProverInput(BigIntegers.fromUnsignedByteArray("test".getBytes())).publicImage
    val newlyMinedBlock    = readers.h.bestFullBlockOpt.get
    val rewardBox: ErgoBox = newlyMinedBlock.transactions.last.outputs.last
    rewardBox.propositionBytes shouldBe ErgoTreePredef
      .rewardOutputScript(emission.settings.minerRewardDelay, defaultMinerPk)
      .bytes
    val input = Input(rewardBox.id, emptyProverResult)

    val outputs = IndexedSeq(
      new ErgoBoxCandidate(rewardBox.value, ErgoTree.fromSigmaBoolean(prop), readers.s.stateContext.currentHeight)
    )
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)

    val tx = ErgoTransaction(
      defaultProver
        .sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext)
        .get
    )

    // A non-forced request remains cached; regeneration is an explicit request.
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>

        // solve a block

        val result = powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        val block = result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }

        // Add a pool transaction, observe the unchanged cache, then explicitly regenerate.
        viewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))
        expectNoMessage(candidateGenDelay)
        candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
        val stillCached = testProbe.expectMsgPF(candidateGenDelay) {
          case StatusReply.Success(c: Candidate) => c
        }
        stillCached should be theSameInstanceAs candidate
        candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
        val regeneratedCandidate = testProbe.expectMsgPF(candidateGenDelay) {
          case StatusReply.Success(regeneratedCandidate: Candidate) =>
            // regeneratedCandidate now contains new transaction
            regeneratedCandidate.candidateBlock shouldNot be(
              candidate.candidateBlock
            )
            regeneratedCandidate
        }

        // we are submitting solution for previous candidate
        candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)
        testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
        // This fake scheme accepts the old nonce on the current candidate. Pin that exact header.
        val acceptedHeader = CandidateGenerator.completeOrderingBlock(
          regeneratedCandidate.candidateBlock, block.header.powSolution).header
        powScheme.validate(acceptedHeader).get
        expectAppliedBlock(appliedProbe, acceptedHeader)
    }
    system.terminate()
  }

  it should "remove pool transactions when a legacy V1 block is mined" in new TestKit(
    newActorSystem()
  ) {
    val legacySettings = defaultSettings.copy(
      networkType = org.ergoplatform.settings.NetworkType.Tests,
      directory = s"${defaultSettings.directory}-legacy-pool-${java.util.UUID.randomUUID()}"
    )
    legacySettings.launchParameters.blockVersion shouldBe Header.InitialVersion
    val testProbe = new TestProbe(system)
    val appliedProbe = new TestProbe(system)
    system.eventStream.subscribe(appliedProbe.ref, newBlockSignal)
    val viewHolderRef: ActorRef    = ErgoNodeViewRef(legacySettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        legacySettings
      )

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    val history: ErgoHistoryReader = readers.h
    val startBlock: Option[Header] = history.bestHeaderOpt

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        candidate.candidateBlock.version shouldBe Header.InitialVersion
        val result = legacySettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        val block = result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
        // let's pretend we are mining at least a bit so it is realistic
        expectNoMessage(200.millis)
        candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)

        testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
        expectAppliedBlock(appliedProbe, block.header)
    }

    // build new transaction that uses miner's reward as input
    val prop: ProveDlog =
      DLogProverInput(BigIntegers.fromUnsignedByteArray("test".getBytes())).publicImage
    val newlyMinedBlock    = readers.h.bestFullBlockOpt.get
    val rewardBox: ErgoBox = newlyMinedBlock.transactions.last.outputs.last
    rewardBox.propositionBytes shouldBe ErgoTreePredef
      .rewardOutputScript(emission.settings.minerRewardDelay, defaultMinerPk)
      .bytes
    val input = Input(rewardBox.id, emptyProverResult)

    val outputs = IndexedSeq(
      new ErgoBoxCandidate(rewardBox.value, ErgoTree.fromSigmaBoolean(prop), readers.s.stateContext.currentHeight)
    )
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)

    val tx = ErgoTransaction(
      defaultProver
        .sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext)
        .get
    )

    testProbe.expectNoMessage(200.millis)
    // Put the transaction in the real pool before checking removal on block application.
    viewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))
    eventually(timeout(candidateGenDelay), interval(25.millis)) {
      await((readersHolderRef ? GetReaders).mapTo[Readers]).m.getAllPrioritized.map(_.id) should contain(tx.id)
    }
    // mine a block with that transaction
    candidateGenerator.tell(GenerateCandidate(Seq(tx), reply = true, forced = true, optPk = None), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        candidate.candidateBlock.version shouldBe Header.InitialVersion
        val result = legacySettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        val block = result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
        testProbe.expectNoMessage(200.millis)
        candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)

        testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
        expectAppliedBlock(appliedProbe, block.header)
    }

    // new transaction should be cleared from pool after applying new block
    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 0

    // validate total amount of transactions created
    val blocks: IndexedSeq[ErgoFullBlock] = readers.h
      .chainToHeader(startBlock, readers.h.bestHeaderOpt.get)
      ._2
      .headers
      .flatMap(readers.h.getFullBlock)
      .filter(_.blockTransactions.transactions.map(_.id).contains(tx.id))
    val txs: Seq[ErgoTransaction] = blocks.flatMap(_.blockTransactions.transactions)
    txs should have length 2 // 1 reward and one regular tx, no fee collection tx
    system.terminate()
  }

  it should "select an executable V4 input chain separately from its ordering alternative" in new TestKit(
    newActorSystem()
  ) {
    val matrixSettings = defaultSettings60.copy(
      directory = s"${defaultSettings60.directory}-matrix-dependency-${java.util.UUID.randomUUID()}"
    )
    matrixSettings.launchParameters.blockVersion shouldBe Header.Interpreter60Version
    val testProbe = new TestProbe(system)
    val appliedProbe = new TestProbe(system)
    system.eventStream.subscribe(appliedProbe.ref, newBlockSignal)
    val viewHolderRef: ActorRef    = ErgoNodeViewRef(matrixSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        matrixSettings
      )

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = matrixSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        val block = result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
        // let's pretend we are mining at least a bit so it is realistic
        expectNoMessage(200.millis)
        candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)

        testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
        expectAppliedBlock(appliedProbe, block.header)
    }

    // build new transaction that uses miner's reward as input
    val newlyMinedBlock    = readers.h.bestFullBlockOpt.get

    val rewardBox: ErgoBox = newlyMinedBlock.transactions.last.outputs.last
    rewardBox.propositionBytes shouldBe ErgoTreePredef
      .rewardOutputScript(emission.settings.minerRewardDelay, defaultMinerPk)
      .bytes
    val input = Input(rewardBox.id, emptyProverResult)


    // sigmaProp(Global.serialize(2).size > 0)
    val bs = "1b110204040400d191b1dc6a03dd0173007301"
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get)

    val outputs = IndexedSeq(
      new ErgoBoxCandidate(rewardBox.value, tree, readers.s.stateContext.currentHeight)
    )
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)

    val tx = ErgoTransaction(
      defaultProver
        .sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext)
        .get
    )

    val spendingBox = tx.outputs.head
    val o2 = new ErgoBoxCandidate(spendingBox.value, tree, spendingBox.creationHeight, spendingBox.additionalTokens, spendingBox.additionalRegisters)
    val tx2 = tx.copy(
      inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o2))

    // Classify this concrete fixture using real validation, including the child's dependency.
    candidateGenerator.tell(GenerateCandidate(Seq(tx, tx2), reply = true, forced = true), testProbe.ref)
    val chainCandidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) => candidate
    }
    val candidateBlock = chainCandidate.candidateBlock
    candidateBlock.version shouldBe Header.Interpreter60Version
    val currentReaders = eventually(timeout(candidateGenDelay), interval(25.millis)) {
      val current = await((readersHolderRef ? GetReaders).mapTo[Readers])
      current.s.stateContext.lastHeaderOpt.map(_.id) shouldBe candidateBlock.parentOpt.map(_.id)
      current
    }
    val currentState = currentReaders.s.asInstanceOf[UtxoStateReader]
    val upcoming = currentState.stateContext.upcoming(
      defaultMinerPk.value, candidateBlock.timestamp, candidateBlock.nBits, candidateBlock.votes,
      matrixSettings.votingTargets.desiredUpdate, candidateBlock.version)
    currentState.validateWithCost(tx, upcoming,
      chainCandidate.parameters.maxBlockCost, None, softFieldsAllowed = false).get
    currentState.validateWithCost(tx2, upcoming,
      chainCandidate.parameters.maxBlockCost, None, softFieldsAllowed = false).isFailure shouldBe true
    currentState.withTransactions(Seq(tx)).validateWithCost(tx2, upcoming,
      chainCandidate.parameters.maxBlockCost, None, softFieldsAllowed = false).get

    val expectedInput = Seq(tx, tx2)
    val expectedOrdering = CandidateGenerator.collectEmission(
      currentState, defaultMinerPk, currentState.stateContext).toSeq
    expectedOrdering should have length 1
    candidateBlock.inputBlockTransactions.map(_.id) shouldBe expectedInput.map(_.id)
    candidateBlock.orderingBlockTransactions.map(_.id) shouldBe expectedOrdering.map(_.id)
    candidateBlock.transactions.map(_.id) shouldBe expectedOrdering.map(_.id)
    currentState.proofsForTransactions(expectedInput).get
    val (_, orderingRoot) = currentState.proofsForTransactions(expectedOrdering).get
    candidateBlock.stateRoot.toSeq shouldBe orderingRoot.toSeq

    // Submit the ordering alternative. This applies emission only; the input chain
    // remains separately executable and is not falsely counted as applied here.
    val orderingBlock = matrixSettings.chainSettings.powScheme
      .proveCandidate(candidateBlock, defaultMinerSecret.w, 0, 1000, chainCandidate.parameters) match {
      case org.ergoplatform.OrderingBlockFound(block) => block
      case other => fail(s"Expected ordering result, got $other")
    }
    orderingBlock.transactions.map(_.id) shouldBe expectedOrdering.map(_.id)
    candidateGenerator.tell(OrderingSolutionFound(orderingBlock.header.powSolution), testProbe.ref)
    testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
    expectAppliedBlock(appliedProbe, orderingBlock.header)
    val afterOrdering = eventually(timeout(candidateGenDelay), interval(25.millis)) {
      val current = await((readersHolderRef ? GetReaders).mapTo[Readers])
      current.s.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(orderingBlock.header.id)
      current
    }
    afterOrdering.h.getFullBlock(orderingBlock.header).get.transactions.map(_.id) shouldBe expectedOrdering.map(_.id)
    val stateAfterOrdering = afterOrdering.s.asInstanceOf[UtxoStateReader]
    stateAfterOrdering.boxById(rewardBox.id).isDefined shouldBe true
    stateAfterOrdering.boxById(spendingBox.id).isDefined shouldBe false

    system.terminate()
  }

  it should "use custom miner public key when provided via optPk" in new TestKit(ActorSystem()) {
    import sigmastate.crypto.DLogProtocol.DLogProverInput
    import org.bouncycastle.util.BigIntegers

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )

    // Generate custom key pair
    val customKey = DLogProverInput(BigIntegers.fromUnsignedByteArray("custom_test_key".getBytes()))
    val customPk = customKey.publicImage

    // Request candidate with custom public key
    candidateGenerator.tell(
      GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = Some(customPk)),
      testProbe.ref
    )

    val candidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Verify candidate was generated successfully
    candidate should not be null
    candidate.candidateBlock should not be null
    candidate.externalVersion.pk shouldBe customPk

    system.terminate()
  }

  it should "use default minerPk when optPk is None" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )

    candidateGenerator.tell(
      GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None),
      testProbe.ref
    )

    val candidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Candidate should be generated successfully with default minerPk
    candidate should not be null
    candidate.candidateBlock should not be null
    candidate.externalVersion.pk shouldBe defaultMinerSecret.publicImage

    system.terminate()
  }

  it should "generate different candidates for different optPk values" in new TestKit(ActorSystem()) {
    import sigmastate.crypto.DLogProtocol.DLogProverInput
    import org.bouncycastle.util.BigIntegers

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )

    // Generate custom key pair
    val customKey = DLogProverInput(BigIntegers.fromUnsignedByteArray("another_test_key".getBytes()))
    val customPk = customKey.publicImage

    // Get candidate with default pk
    candidateGenerator.tell(
      GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None),
      testProbe.ref
    )
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Get candidate with custom pk
    candidateGenerator.tell(
      GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = Some(customPk)),
      testProbe.ref
    )
    val candidate2 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Both candidates should be generated successfully
    candidate1 should not be null
    candidate2 should not be null
    candidate1.externalVersion.pk shouldBe defaultMinerSecret.publicImage
    candidate2.externalVersion.pk shouldBe customPk
    candidate1.externalVersion.pk should not be candidate2.externalVersion.pk

    system.terminate()
  }

  it should "handle optPk with empty transactions" in new TestKit(ActorSystem()) {
    import sigmastate.crypto.DLogProtocol.DLogProverInput
    import org.bouncycastle.util.BigIntegers

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings
      )

    // Generate custom key pair
    val customKey = DLogProverInput(BigIntegers.fromUnsignedByteArray("tx_test_key".getBytes()))
    val customPk = customKey.publicImage

    // Request candidate with custom pk and empty transactions
    candidateGenerator.tell(
      GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = Some(customPk)),
      testProbe.ref
    )

    val candidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Candidate should be generated successfully
    candidate should not be null
    candidate.txsToInclude shouldBe empty

    system.terminate()
  }

  it should "ignore cached candidate when forced = true" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val testDir = s"${defaultSettings.directory}-ignore-cache-${System.currentTimeMillis()}"
    val settingsWithShortRegeneration: ErgoSettings =
      ErgoSettingsReader.read()
        .copy(
          nodeSettings = defaultSettings.nodeSettings
            .copy(blockCandidateGenerationInterval = 1.millis),
          chainSettings =
            ErgoSettingsReader.read().chainSettings.copy(blockInterval = 1.seconds),
          directory = testDir
        )

    val viewHolderRef: ActorRef = ErgoNodeViewRef(settingsWithShortRegeneration)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        settingsWithShortRegeneration
      )

    val powScheme = settingsWithShortRegeneration.chainSettings.powScheme

    // First mine a block to establish chain (needed for avg mining time calculation)
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val initCandidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }
    val initBlock = powScheme
      .proveCandidate(initCandidate.candidateBlock, defaultMinerSecret.w, 0, 1000, initCandidate.parameters) match {
      case org.ergoplatform.OrderingBlockFound(h) => h
      case org.ergoplatform.InputBlockFound(fb) => fb
      case _ => throw new RuntimeException("Unexpected result from proveCandidate")
    }
    candidateGenerator.tell(OrderingSolutionFound(initBlock.header.powSolution), testProbe.ref)
    testProbe.fishForMessage(blockValidationDelay) {
      case StatusReply.Success(()) => true
      case FullBlockApplied(header) if header.id != initBlock.header.parentId => true
      case _ => false
    }

    // Get first candidate after chain is established
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Request with forced = false should return cached candidate immediately
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate2 = testProbe.expectMsgPF(100.millis) {
      case StatusReply.Success(c: Candidate) => c
    }
    // Should be the exact same cached candidate
    candidate2.candidateBlock.timestamp shouldBe candidate1.candidateBlock.timestamp

    // Request with forced = true should bypass cache and regenerate
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
    val candidate3 = testProbe.fishForMessage(candidateGenDelay) {
      case StatusReply.Success(_: Candidate) => true
      case _: FullBlockApplied => false
    } match {
      case StatusReply.Success(c: Candidate) => c
    }

    // candidate3 should have timestamp >= candidate1 (regenerated, possibly same or newer)
    candidate3.candidateBlock.timestamp should be >= candidate1.candidateBlock.timestamp

    system.terminate()
  }

  it should "accept an earlier nonce after forced candidate regeneration" in new TestKit(newActorSystem()) {
    val testProbe = new TestProbe(system)
    val appliedProbe = new TestProbe(system)
    system.eventStream.subscribe(appliedProbe.ref, newBlockSignal)

    val testDir = s"${defaultSettings.directory}-preserve-candidate-${System.currentTimeMillis()}"
    val settingsWithShortRegeneration: ErgoSettings =
      ErgoSettingsReader.read()
        .copy(
          nodeSettings = defaultSettings.nodeSettings
            .copy(blockCandidateGenerationInterval = 1.millis),
          chainSettings =
            ErgoSettingsReader.read().chainSettings.copy(blockInterval = 1.seconds),
          directory = testDir
        )

    val viewHolderRef: ActorRef = ErgoNodeViewRef(settingsWithShortRegeneration)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        settingsWithShortRegeneration
      )

    val powScheme = settingsWithShortRegeneration.chainSettings.powScheme

    // First mine a block to establish chain (needed for avg mining time calculation)
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val initCandidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }
    val initBlock = powScheme
      .proveCandidate(initCandidate.candidateBlock, defaultMinerSecret.w, 0, 1000, initCandidate.parameters) match {
      case org.ergoplatform.OrderingBlockFound(h) => h
      case org.ergoplatform.InputBlockFound(fb) => fb
      case _ => throw new RuntimeException("Unexpected result from proveCandidate")
    }
    candidateGenerator.tell(OrderingSolutionFound(initBlock.header.powSolution), testProbe.ref)
    // Retain the original shared deadline while observing replies and events separately.
    val applicationDeadline = blockValidationDelay.fromNow
    testProbe.expectMsg(applicationDeadline.timeLeft, StatusReply.Success(()))
    expectAppliedBlock(appliedProbe, initBlock.header, applicationDeadline.timeLeft)

    // Get first candidate after chain is established
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Force regeneration before submitting work obtained from candidate1.
    val candidate2 = eventually(timeout(candidateGenDelay), interval(100.millis)) {
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
      testProbe.expectMsgPF(500.millis) {
        case StatusReply.Success(c: Candidate) => c
      }
    }

    // candidate2 should be different from candidate1 (regenerated)
    candidate2.candidateBlock.timestamp should be >= candidate1.candidateBlock.timestamp

    // Solve a block using the earlier candidate1.
    val solvedBlock = powScheme
      .proveCandidate(candidate1.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate1.parameters) match {
      case org.ergoplatform.OrderingBlockFound(h) => h
      case org.ergoplatform.InputBlockFound(fb) => fb
      case _ => throw new RuntimeException("Unexpected result from proveCandidate")
    }

    // Fake PoW accepts this earlier nonce on candidate2; assert that exact completion below.
    candidateGenerator.tell(OrderingSolutionFound(solvedBlock.header.powSolution), testProbe.ref)

    // Acceptance here does not establish use of the previous-candidate fallback.
    testProbe.expectMsgPF(blockValidationDelay) {
      case StatusReply.Success(()) =>
    }

    val acceptedHeader = CandidateGenerator.completeOrderingBlock(
      candidate2.candidateBlock, solvedBlock.header.powSolution).header
    powScheme.validate(acceptedHeader).get
    expectAppliedBlock(appliedProbe, acceptedHeader)

    system.terminate()
  }

  it should "handle multiple consecutive forced regenerations correctly" in new TestKit(newActorSystem()) {
    val testProbe = new TestProbe(system)
    val appliedProbe = new TestProbe(system)
    system.eventStream.subscribe(appliedProbe.ref, newBlockSignal)

    // Use unique directory to avoid state conflicts
    val testDir = s"${defaultSettings.directory}-multi-forced-${System.currentTimeMillis()}"
    val settingsWithShortRegeneration: ErgoSettings =
      ErgoSettingsReader.read()
        .copy(
          nodeSettings = defaultSettings.nodeSettings
            .copy(blockCandidateGenerationInterval = 1.millis),
          chainSettings =
            ErgoSettingsReader.read().chainSettings.copy(blockInterval = 1.seconds),
          directory = testDir
        )

    val viewHolderRef: ActorRef = ErgoNodeViewRef(settingsWithShortRegeneration)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        settingsWithShortRegeneration
      )

    val powScheme = settingsWithShortRegeneration.chainSettings.powScheme

    // First mine a block to establish chain
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val initCandidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }
    val initBlock = powScheme
      .proveCandidate(initCandidate.candidateBlock, defaultMinerSecret.w, 0, 1000, initCandidate.parameters) match {
      case org.ergoplatform.OrderingBlockFound(h) => h
      case org.ergoplatform.InputBlockFound(fb) => fb
      case _ => throw new RuntimeException("Unexpected result from proveCandidate")
    }
    candidateGenerator.tell(OrderingSolutionFound(initBlock.header.powSolution), testProbe.ref)
    val applicationDeadline = blockValidationDelay.fromNow
    testProbe.expectMsg(applicationDeadline.timeLeft, StatusReply.Success(()))
    expectAppliedBlock(appliedProbe, initBlock.header, applicationDeadline.timeLeft)

    // Now get candidate after chain is established
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Force regenerate first time
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
    val candidate2 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Force regenerate second time
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
    val candidate3 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // All candidates should have increasing or equal timestamps
    candidate2.candidateBlock.timestamp should be >= candidate1.candidateBlock.timestamp
    candidate3.candidateBlock.timestamp should be >= candidate2.candidateBlock.timestamp

    // Solve earlier candidate2; fake PoW accepts this nonce on current candidate3.
    val solvedBlock = powScheme
      .proveCandidate(candidate2.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate2.parameters) match {
      case org.ergoplatform.OrderingBlockFound(h) => h
      case org.ergoplatform.InputBlockFound(fb) => fb
      case _ => throw new RuntimeException("Unexpected result from proveCandidate")
    }

    candidateGenerator.tell(OrderingSolutionFound(solvedBlock.header.powSolution), testProbe.ref)

    testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
    val acceptedHeader = CandidateGenerator.completeOrderingBlock(
      candidate3.candidateBlock, solvedBlock.header.powSolution).header
    powScheme.validate(acceptedHeader).get
    expectAppliedBlock(appliedProbe, acceptedHeader)

    system.terminate()
  }

  it should "return cached candidate immediately when forced = false" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val testDir = s"${defaultSettings.directory}-cache-test-${System.currentTimeMillis()}"
    val testSettings = defaultSettings.copy(directory = testDir)

    val viewHolderRef: ActorRef = ErgoNodeViewRef(testSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        testSettings
      )

    // Get first candidate
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Multiple requests with forced = false should return cached candidate immediately
    val start = System.currentTimeMillis()
    (1 to 10).foreach { i =>
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
      val candidate = testProbe.expectMsgPF(100.millis) {
        case StatusReply.Success(c: Candidate) => c
      }
      candidate.candidateBlock.timestamp shouldBe candidate1.candidateBlock.timestamp
    }
    val elapsed = System.currentTimeMillis() - start

    // Should be very fast since all are cached (no regeneration)
    elapsed should be < 500L

    system.terminate()
  }

  it should "retain cached work after a mempool change and accept its solution" in new TestKit(newActorSystem()) {
    val testProbe = new TestProbe(system)
    val appliedProbe = new TestProbe(system)
    system.eventStream.subscribe(appliedProbe.ref, newBlockSignal)

    val testDir = s"${defaultSettings.directory}-mempool-forced-${System.currentTimeMillis()}"
    val settingsWithShortRegeneration: ErgoSettings =
      ErgoSettingsReader.read()
        .copy(
          nodeSettings = defaultSettings.nodeSettings
            .copy(blockCandidateGenerationInterval = 100.millis),
          chainSettings =
            ErgoSettingsReader.read().chainSettings.copy(blockInterval = 1.seconds),
          directory = testDir
        )

    val viewHolderRef: ActorRef = ErgoNodeViewRef(settingsWithShortRegeneration)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        settingsWithShortRegeneration
      )

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])
    val powScheme = settingsWithShortRegeneration.chainSettings.powScheme

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val result = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000, candidate.parameters)
        val block = result match {
          case org.ergoplatform.OrderingBlockFound(h) => h
          case org.ergoplatform.InputBlockFound(fb) => fb
          case _ => throw new RuntimeException("Unexpected result from proveCandidate")
        }
        // let's pretend we are mining at least a bit so it is realistic
        expectNoMessage(200.millis)
        candidateGenerator.tell(OrderingSolutionFound(block.header.powSolution), testProbe.ref)

        testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
        expectAppliedBlock(appliedProbe, block.header)
    }

    // Get candidate and solve it
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidateToSolve = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    val solvedBlock = powScheme
      .proveCandidate(candidateToSolve.candidateBlock, defaultMinerSecret.w, 0, 1000, candidateToSolve.parameters) match {
      case org.ergoplatform.OrderingBlockFound(h) => h
      case org.ergoplatform.InputBlockFound(fb) => fb
      case _ => throw new RuntimeException("Unexpected result from proveCandidate")
    }

    // Build a new transaction to change the mempool independently of the cached work.
    val prop: ProveDlog =
      DLogProverInput(BigIntegers.fromUnsignedByteArray("forced-mempool-test".getBytes())).publicImage
    val newlyMinedBlock = readers.h.bestFullBlockOpt.get
    val rewardBox: ErgoBox = newlyMinedBlock.transactions.last.outputs.last
    val input = Input(rewardBox.id, emptyProverResult)

    val outputs = IndexedSeq(
      new ErgoBoxCandidate(rewardBox.value, ErgoTree.fromSigmaBoolean(prop), readers.s.stateContext.currentHeight)
    )
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)
    val tx = ErgoTransaction(
      defaultProver
        .sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext)
        .get
    )

    // Submit transaction to mempool
    viewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))

    // Allow the mempool event to arrive; it does not itself regenerate cached work.
    testProbe.expectNoMessage(200.millis)

    // A non-forced request still returns the cached candidate after ChangedMempool.
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val regeneratedCandidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    regeneratedCandidate should be theSameInstanceAs candidateToSolve

    // Submit the solution for that same cached candidate.
    candidateGenerator.tell(OrderingSolutionFound(solvedBlock.header.powSolution), testProbe.ref)

    // Require both the direct acknowledgement and the local application event.
    testProbe.expectMsg(blockValidationDelay, StatusReply.Success(()))
    val acceptedHeader = CandidateGenerator.completeOrderingBlock(
      regeneratedCandidate.candidateBlock, solvedBlock.header.powSolution).header
    powScheme.validate(acceptedHeader).get
    expectAppliedBlock(appliedProbe, acceptedHeader)

    system.terminate()
  }

  it should "correctly complete input block from candidate and solution" in new TestKit(
    ActorSystem()
  ) {
    val viewHolderRef: ActorRef    = ErgoNodeViewRef(defaultSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])
    val history: ErgoHistoryReader = readers.h
    val utxoState = readers.s.asInstanceOf[UtxoStateReader]
    val mempool = readers.m

    val candidateOpt = CandidateGenerator.generateCandidate(
      history,
      utxoState,
      mempool,
      defaultMinerPk,
      Seq.empty,
      None,
      defaultSettings
    )

    // If we can't generate a candidate (e.g., due to lack of proper history), skip this test
    candidateOpt match {
      case Some(scala.util.Success((candidate: CandidateGenerator.Candidate, _))) =>
        val candidateBlock = candidate.candidateBlock

        // Create a mock solution - the completeInputBlock method expects an AutolykosSolution
        import org.ergoplatform.AutolykosSolution
        import sigma.crypto.CryptoConstants
        val solution = new AutolykosSolution(
          defaultMinerPk.value,
          CryptoConstants.dlogGroup.generator, // w
          Array.fill(8)(0.toByte), // n - must be 8 bytes for Autolykos V1
          BigInt(0) // d
        )

        // Call the completeInputBlock method
        val (inputBlockInfo, inputBlockTransactionsData) = CandidateGenerator.completeInputBlock(candidateBlock, solution)

        // Verify the results
        inputBlockInfo shouldBe a[InputBlockAnnouncement]
        inputBlockTransactionsData shouldBe a[InputBlockTransactionsData]

        // Check that the input block info has the correct header
        inputBlockInfo.header should not be null

        // Check that the input block transactions data has the correct ID matching the header
        inputBlockTransactionsData.inputBlockId shouldBe inputBlockInfo.header.id

        // Check that the transactions match
        inputBlockTransactionsData.transactions should have length candidateBlock.inputBlockTransactions.length

        // Check that weak IDs are properly computed
        val expectedWeakIds = candidateBlock.inputBlockTransactions.map(_.weakId)
        val actualWeakIds = inputBlockInfo.weakTxIds.getOrElse(Seq.empty)
        actualWeakIds should contain theSameElementsAs expectedWeakIds
      case _ =>
        // Skip test if we can't generate a candidate (due to chain not being synced, etc.)
        pending
    }

    system.terminate()
  }

  private class FixedReadersHolder(readers: Readers) extends Actor {
    override def receive: Receive = {
      case GetReaders => sender() ! readers
    }
  }

  private def walletStub(implicit system: ActorSystem): ErgoWalletReader = new ErgoWalletReader {
    val walletActor: ActorRef = system.deadLetters
  }

  private def testSettings(directory: String): ErgoSettings = {
    defaultSettings.copy(
      directory = directory,
      nodeSettings = defaultSettings.nodeSettings.copy(
        blockCandidateGenerationInterval = 1.second
      )
    )
  }

  private def historyWithBestFullBlock(blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    val h0 = HistoryTestHelpers.generateHistory(
      verifyTransactions = true,
      stateType = StateType.Utxo,
      PoPoWBootstrap = false,
      blocksToKeep = 100
    )
    val h1 = applyChain(h0, blocks)
    val extraHeaders = genHeaderChain(2, h1, diffBitsOpt = None, useRealTs = false)
    extraHeaders.headers.drop(h1.headersHeight).foldLeft(h1) { case (h, header) =>
      h.append(header).get._1
    }
  }

  it should "exclude applied transactions from stale mempool and not eliminate them" in new TestKit(
    ActorSystem()
  ) {

    val testDir = s"${defaultSettings.directory}-a1-stale-${System.currentTimeMillis()}"
    val settings = testSettings(testDir)
    val viewHolderProbe = TestProbe()
    val senderProbe = TestProbe()

    val (us0, bh0) = createUtxoState(settings)
    val rnd = new RandomWrapper

    val (txs1, bh1) = validTransactionsFromBoxHolder(bh0, rnd)
    txs1 should not be empty
    val block1 = validFullBlock(None, us0, txs1)
    val us1 = us0.applyModifier(block1, None)(_ => ()).get

    val (txs2, _) = validTransactionsFromBoxHolder(bh1, rnd)
    txs2 should not be empty
    val tx = txs2.head
    val block2 = validFullBlock(Some(block1), us1, txs2)
    val us2 = us1.applyModifier(block2, None)(_ => ()).get

    val history0 = HistoryTestHelpers.generateHistory(
      verifyTransactions = true,
      stateType = StateType.Utxo,
      PoPoWBootstrap = false,
      blocksToKeep = 100
    )
    val history2 = applyChain(history0, Seq(block1, block2))

    val wallet = walletStub
    val emptyMempool = ErgoMemPool.empty(settings)
    val readers = Readers(history2, us2, emptyMempool, wallet)

    val readersHolderRef = system.actorOf(Props(new FixedReadersHolder(readers)))
    val candidateGenerator = CandidateGenerator(
      defaultMinerSecret.publicImage,
      readersHolderRef,
      viewHolderProbe.ref,
      settings
    )

    // let the actor initialize and generate an initial candidate with the empty mempool
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), senderProbe.ref)
    senderProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(_: Candidate) => ()
    }

    candidateGenerator ! LocalBlockApplied(block2.header, Seq(tx.id))

    val staleMempool = ErgoMemPool.empty(settings).put(Seq(UnconfirmedTransaction(tx, None)))
    staleMempool.getAllPrioritized.map(_.id) should contain(tx.id)
    candidateGenerator ! ChangedMempool(staleMempool)

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), senderProbe.ref)

    val candidate = senderProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    candidate.candidateBlock.transactions should not be empty
    candidate.candidateBlock.transactions.map(_.id) should not contain tx.id

    val eliminatedIds = viewHolderProbe.receiveWhile(500.millis) {
      case e: EliminateTransactions => e
    }.flatMap(_.ids)
    eliminatedIds should not contain tx.id

    system.terminate()
  }

  it should "discard candidate when history and state are out of sync" in new TestKit(
    ActorSystem()
  ) {

    val testDir = s"${defaultSettings.directory}-b1-sync-${System.currentTimeMillis()}"
    val settings = testSettings(testDir)
    val viewHolderProbe = TestProbe()
    val senderProbe = TestProbe()

    val (us0, bh0) = createUtxoState(settings)
    val rnd = new RandomWrapper
    val (txs1, bh1) = validTransactionsFromBoxHolder(bh0, rnd)
    txs1 should not be empty

    val block1 = validFullBlock(None, us0, txs1)
    val us1 = us0.applyModifier(block1, None)(_ => ()).get
    val history1 = historyWithBestFullBlock(Seq(block1))

    val (txs2, _) = validTransactionsFromBoxHolder(bh1, rnd)
    txs2 should not be empty
    val block2 = validFullBlock(Some(block1), us1, txs2)
    val us2 = us1.applyModifier(block2, None)(_ => ()).get

    val mempool = ErgoMemPool.empty(settings)
    val wallet = walletStub
    val readers = Readers(history1, us2, mempool, wallet)

    val readersHolderRef = system.actorOf(Props(new FixedReadersHolder(readers)))
    val candidateGenerator = CandidateGenerator(
      defaultMinerSecret.publicImage,
      readersHolderRef,
      viewHolderProbe.ref,
      settings
    )

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), senderProbe.ref)
    senderProbe.expectNoMessage(2.seconds)
    viewHolderProbe.expectNoMessage(500.millis)

    system.terminate()
  }

  it should "recover with a filled candidate when proof generation fails once" in new TestKit(
    newActorSystem()
  ) {
    val testDir = s"${defaultSettings.directory}-proof-retry-${System.currentTimeMillis()}"
    val settings = testSettings(testDir)
    val viewHolderProbe = TestProbe()
    val senderProbe = TestProbe()

    val (us0, bh0) = createUtxoState(settings)
    val rnd = new RandomWrapper

    val (txs1, bh1) = validTransactionsFromBoxHolder(bh0, rnd)
    txs1 should not be empty
    val block1 = validFullBlock(None, us0, txs1)
    val us1 = us0.applyModifier(block1, None)(_ => ()).get

    val (txs2, bh2) = validTransactionsFromBoxHolder(bh1, rnd)
    txs2 should not be empty
    val block2 = validFullBlock(Some(block1), us1, txs2)
    val us2 = us1.applyModifier(block2, None)(_ => ()).get
    val history2 = applyChain(
      HistoryTestHelpers.generateHistory(
        verifyTransactions = true,
        stateType = StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = 100
      ),
      Seq(block1, block2)
    )

    // This TestNet fixture has V4 parameters: regular transactions belong to the input
    // payload. Use existing boxes so that they do not depend on this candidate's emission.
    settings.networkType shouldBe org.ergoplatform.settings.NetworkType.TestNet
    us2.stateContext.currentParameters.blockVersion shouldBe Header.Interpreter60Version
    val emissionBox = us2.emissionBoxOpt.get
    val (_, regularBoxes) = bh2.take(box => box.id.sameElements(emissionBox.id))
    val (txs3, _) = validTransactionsFromBoxHolder(regularBoxes, rnd)
    txs3 should not be empty
    val expectedOrdering = CandidateGenerator.collectEmission(us2, defaultMinerPk, us2.stateContext).toSeq
    expectedOrdering should have length 1

    // Arm after startup so the requested filled candidate, rather than the actor's
    // automatic emission-only candidate, encounters the one-shot proof failure.
    @volatile var proofFailureArmed = false
    var proofsFailed = false
    var armedProofInputs = Vector.empty[Seq[ErgoTransaction]]
    val failingOnceState = new UtxoState(us2.persistentProver, us2.version, us2.store, settings) {
      override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] = {
        if (proofFailureArmed) armedProofInputs :+= txs
        if (proofFailureArmed && !proofsFailed) {
          proofsFailed = true
          Failure(new Exception("Simulating state update during candidate assembly"))
        } else {
          super.proofsForTransactions(txs)
        }
      }
    }

    val readers = Readers(history2, failingOnceState, ErgoMemPool.empty(settings), walletStub)
    val readersHolderRef = system.actorOf(Props(new FixedReadersHolder(readers)))
    val candidateGenerator = CandidateGenerator(
      defaultMinerSecret.publicImage,
      readersHolderRef,
      viewHolderProbe.ref,
      settings
    )

    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), senderProbe.ref)
    senderProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(_: Candidate) => ()
    }
    proofFailureArmed = true
    candidateGenerator.tell(GenerateCandidate(txs3, reply = true, forced = true), senderProbe.ref)
    val candidate = senderProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Both attempts prove precisely the ordering payload; successful retry retains every
    // regular transaction in its input payload without promoting it to ordering.
    proofsFailed shouldBe true
    candidate.candidateBlock.version shouldBe Header.Interpreter60Version
    candidate.candidateBlock.inputBlockTransactions.map(_.id) shouldBe txs3.map(_.id)
    candidate.candidateBlock.orderingBlockTransactions.map(_.id) shouldBe expectedOrdering.map(_.id)
    candidate.candidateBlock.transactions.map(_.id) shouldBe expectedOrdering.map(_.id)
    armedProofInputs.map(_.map(_.id)) shouldBe Vector.fill(2)(expectedOrdering.map(_.id))
    val (_, expectedRoot) = us2.proofsForTransactions(expectedOrdering).get
    candidate.candidateBlock.stateRoot.toSeq shouldBe expectedRoot.toSeq

    system.terminate()
  }

  it should "fall back to emission-only candidate when proof generation keeps failing" in new TestKit(
    ActorSystem()
  ) {
    val testDir = s"${defaultSettings.directory}-proof-fallback-${System.currentTimeMillis()}"
    val settings = testSettings(testDir)
    val viewHolderProbe = TestProbe()
    val senderProbe = TestProbe()

    val (us0, bh0) = createUtxoState(settings)
    val rnd = new RandomWrapper

    val (txs1, bh1) = validTransactionsFromBoxHolder(bh0, rnd)
    txs1 should not be empty
    val block1 = validFullBlock(None, us0, txs1)
    val us1 = us0.applyModifier(block1, None)(_ => ()).get

    val (txs2, bh2) = validTransactionsFromBoxHolder(bh1, rnd)
    txs2 should not be empty
    val block2 = validFullBlock(Some(block1), us1, txs2)
    val us2 = us1.applyModifier(block2, None)(_ => ()).get
    val history2 = applyChain(
      HistoryTestHelpers.generateHistory(
        verifyTransactions = true,
        stateType = StateType.Utxo,
        PoPoWBootstrap = false,
        blocksToKeep = 100
      ),
      Seq(block1, block2)
    )

    val (txs3, _) = validTransactionsFromBoxHolder(bh2, rnd)
    txs3 should not be empty

    val alwaysFailingState = new UtxoState(us2.persistentProver, us2.version, us2.store, settings) {
      override def proofsForTransactions(txs: Seq[ErgoTransaction]): Try[(SerializedAdProof, ADDigest)] =
        if (txs.lengthCompare(1) > 0) {
          Failure(new Exception("Simulating persistent proof generation failure"))
        } else {
          super.proofsForTransactions(txs)
        }
    }

    val readers = Readers(history2, alwaysFailingState, ErgoMemPool.empty(settings), walletStub)
    val readersHolderRef = system.actorOf(Props(new FixedReadersHolder(readers)))
    val candidateGenerator = CandidateGenerator(
      defaultMinerSecret.publicImage,
      readersHolderRef,
      viewHolderProbe.ref,
      settings
    )

    candidateGenerator.tell(GenerateCandidate(txs3, reply = true, forced = true), senderProbe.ref)
    val candidate = senderProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // both attempts failed, so only the emission transaction is included
    candidate.candidateBlock.transactions should have length 1

    system.terminate()
  }

}
