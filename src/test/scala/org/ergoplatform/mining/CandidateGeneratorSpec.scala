package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.{StatusReply, ask}
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction, UnsignedErgoTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, FullBlockApplied, LocalBlockApplied}

import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{EliminateTransactions, LocallyGeneratedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.NetworkType.DevNet60
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock, validTransactionsFromBoxHolder}
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genHeaderChain}
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef, Input}
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import sigma.ast.ErgoTree
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigma.data.ProveDlog
import sigma.serialization.ErgoTreeSerializer
import sigmastate.crypto.DLogProtocol.DLogProverInput

import scala.concurrent.duration._

class CandidateGeneratorSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers with Eventually {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  implicit private val timeout: Timeout = defaultTimeout

  private val newBlockSignal: Class[FullBlockApplied] = classOf[FullBlockApplied]
  private val newBlockDelay: FiniteDuration        = 30.seconds
  private val candidateGenDelay: FiniteDuration    = 3.seconds
  private val blockValidationDelay: FiniteDuration = 2.seconds

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

  it should "let multiple miners compete" in new TestKit(ActorSystem()) {
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

    m1.tell(ErgoMiningThread.GetSolvedBlocksCount, testProbe.ref)

    val m1Count =
      testProbe.expectMsgClass(50.millis, classOf[ErgoMiningThread.SolvedBlocksCount])
    m2.tell(ErgoMiningThread.GetSolvedBlocksCount, testProbe.ref)

    val m2Count =
      testProbe.expectMsgClass(50.millis, classOf[ErgoMiningThread.SolvedBlocksCount])
    m3.tell(ErgoMiningThread.GetSolvedBlocksCount, testProbe.ref)

    val m3Count =
      testProbe.expectMsgClass(50.millis, classOf[ErgoMiningThread.SolvedBlocksCount])

    List(m1Count, m2Count, m3Count).map(_.count).sum should be >= 3
    system.terminate()
  }

  it should "cache candidate until newly mined block is applied" in new TestKit(
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

    expectNoMessage(1.second)
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)

    val block = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
    }

    // now block should be cached
    (0 to 20).foreach { _ =>
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
      testProbe.expectMsgClass(5.millis, classOf[StatusReply[_]])
    }

    candidateGenerator.tell(block.header.powSolution, testProbe.ref)
    // we fish either for ack or SSM as the order is non-deterministic
    testProbe.fishForMessage(blockValidationDelay) {
      case StatusReply.Success(()) =>
        testProbe.expectMsgPF(candidateGenDelay) {
          case FullBlockApplied(header) if header.id != block.header.parentId =>
        }
        true
      case FullBlockApplied(header) if header.id != block.header.parentId =>
        testProbe.expectMsg(StatusReply.Success(()))
        true
    }

    system.terminate()
  }

  it should "regenerate candidate periodically" in new TestKit(
    ActorSystem()
  ) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

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

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = settingsWithShortRegeneration.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)
        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
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

    // candidate should be regenerated immediately after a mempool change
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        // this triggers mempool change that triggers candidate regeneration
        viewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))
        expectNoMessage(candidateGenDelay)
        candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
        testProbe.expectMsgPF(candidateGenDelay) {
          case StatusReply.Success(regeneratedCandidate: Candidate) =>
            // regeneratedCandidate now contains new transaction
            regeneratedCandidate.candidateBlock shouldNot be(
              candidate.candidateBlock
            )
        }
    }
    system.terminate()
  }

  it should "accept solution for previous candidate after regeneration" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)
        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
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

    // candidate should be regenerated immediately after a mempool change
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>

        // solve a block

        val block = powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get

        // this triggers mempool change that triggers candidate regeneration
        viewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))
        expectNoMessage(candidateGenDelay)
        candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
        testProbe.expectMsgPF(candidateGenDelay) {
          case StatusReply.Success(regeneratedCandidate: Candidate) =>
            // regeneratedCandidate now contains new transaction
            regeneratedCandidate.candidateBlock shouldNot be(
              candidate.candidateBlock
            )
        }

        // we are submitting solution for previous candidate
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)
        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
    }
    system.terminate()
  }

  it should "pool transactions should be removed from pool when block is mined" in new TestKit(
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

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    val history: ErgoHistoryReader = readers.h
    val startBlock: Option[Header] = history.bestHeaderOpt

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        // let's pretend we are mining at least a bit so it is realistic
        expectNoMessage(200.millis)
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)

        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
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
    // mine a block with that transaction
    candidateGenerator.tell(GenerateCandidate(Seq(tx), reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        testProbe.expectNoMessage(200.millis)
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)

        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
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

  it should "6.0 pool transactions should be added to 6.0 block" in new TestKit(
    ActorSystem()
  ) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)
    val viewHolderRef: ActorRef    = ErgoNodeViewRef(defaultSettings60)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)

    val candidateGenerator: ActorRef =
      CandidateGenerator(
        defaultMinerSecret.publicImage,
        readersHolderRef,
        viewHolderRef,
        defaultSettings60
      )

    val readers: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    val history: ErgoHistoryReader = readers.h
    val startBlock: Option[Header] = history.bestHeaderOpt

    // generate block to use reward as our tx input
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        // let's pretend we are mining at least a bit so it is realistic
        expectNoMessage(200.millis)
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)

        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
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

    testProbe.expectNoMessage(200.millis)
    // mine a block with that transaction
    candidateGenerator.tell(GenerateCandidate(Seq(tx, tx2), reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        testProbe.expectNoMessage(200.millis)
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)

        // we fish either for ack or SSM as the order is non-deterministic
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
    }

    // new transactions should be cleared from pool after applying new block
    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 0

    // validate total amount of transactions created
    val blocks: IndexedSeq[ErgoFullBlock] = readers.h
      .chainToHeader(startBlock, readers.h.bestHeaderOpt.get)
      ._2
      .headers
      .flatMap(readers.h.getFullBlock)
      .filter(_.blockTransactions.transactions.map(_.id).contains(tx.id))

    val txs: Seq[ErgoTransaction] = blocks.flatMap(_.blockTransactions.transactions)

    txs should have length 3 // 1 rewards and two regular txs, no fee collection

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
      .proveCandidate(initCandidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .get
    candidateGenerator.tell(initBlock.header.powSolution, testProbe.ref)
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

  it should "preserve previous candidate when forced regeneration occurs" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

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
      .proveCandidate(initCandidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .get
    candidateGenerator.tell(initBlock.header.powSolution, testProbe.ref)
    // Wait for both the direct mining ACK and the async local block application
    var ackSeen = false
    var appliedSeen = false
    testProbe.fishForMessage(blockValidationDelay) {
      case StatusReply.Success(()) =>
        ackSeen = true
        ackSeen && appliedSeen
      case FullBlockApplied(header) if header.id == initBlock.header.id =>
        appliedSeen = true
        ackSeen && appliedSeen
      case _ => false
    }

    // Get first candidate after chain is established
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Force regeneration - this should preserve candidate1 as cachedPreviousCandidate
    val candidate2 = eventually(timeout(candidateGenDelay), interval(100.millis)) {
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
      testProbe.expectMsgPF(500.millis) {
        case StatusReply.Success(c: Candidate) => c
      }
    }

    // candidate2 should be different from candidate1 (regenerated)
    candidate2.candidateBlock.timestamp should be >= candidate1.candidateBlock.timestamp

    // Solve a block using candidate1 (the "previous" candidate)
    val solvedBlock = powScheme
      .proveCandidate(candidate1.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .get

    // Submit solution - should succeed because candidate1 should be in cachedPreviousCandidate
    candidateGenerator.tell(solvedBlock.header.powSolution, testProbe.ref)

    // CandidateGenerator should accept the solution against cachedPreviousCandidate.
    testProbe.expectMsgPF(blockValidationDelay) {
      case StatusReply.Success(()) =>
    }

    system.terminate()
  }

  it should "handle multiple consecutive forced regenerations correctly" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

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
      .proveCandidate(initCandidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .get
    candidateGenerator.tell(initBlock.header.powSolution, testProbe.ref)
    // Wait for both StatusReply and FullBlockApplied messages
    testProbe.fishForMessage(blockValidationDelay) {
      case StatusReply.Success(()) => true
      case _: FullBlockApplied => true
      case _ => false
    }
    // Try to consume the second message if it exists
    try {
      testProbe.expectMsgClass(1.second, classOf[Any])
    } catch {
      case _: AssertionError => // No more messages, that's fine
    }

    // Now get candidate after chain is established
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidate1 = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Force regenerate first time
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
    val candidate2 = testProbe.fishForMessage(candidateGenDelay) {
      case StatusReply.Success(_: Candidate) => true
      case _: FullBlockApplied => false
    } match {
      case StatusReply.Success(c: Candidate) => c
    }

    // Force regenerate second time
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), testProbe.ref)
    val candidate3 = testProbe.fishForMessage(candidateGenDelay) {
      case StatusReply.Success(_: Candidate) => true
      case _: FullBlockApplied => false
    } match {
      case StatusReply.Success(c: Candidate) => c
    }

    // All candidates should have increasing or equal timestamps
    candidate2.candidateBlock.timestamp should be >= candidate1.candidateBlock.timestamp
    candidate3.candidateBlock.timestamp should be >= candidate2.candidateBlock.timestamp

    // Solve block with candidate2 (should be in cachedPreviousCandidate after candidate3 generation)
    val solvedBlock = powScheme
      .proveCandidate(candidate2.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .get

    candidateGenerator.tell(solvedBlock.header.powSolution, testProbe.ref)

    // Should successfully apply the block
    testProbe.fishForMessage(blockValidationDelay) {
      case StatusReply.Success(()) => true
      case _: FullBlockApplied => true
      case _ => false
    }

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

  it should "accept solution for previous candidate after forced regeneration triggered by mempool" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        candidateGenerator.tell(block.header.powSolution, testProbe.ref)
        testProbe.fishForMessage(blockValidationDelay) {
          case StatusReply.Success(()) =>
            testProbe.expectMsgPF(candidateGenDelay) {
              case FullBlockApplied(header) if header.id != block.header.parentId =>
            }
            true
          case FullBlockApplied(header) if header.id != block.header.parentId =>
            testProbe.expectMsg(StatusReply.Success(()))
            true
        }
    }

    // Get candidate and solve it
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val candidateToSolve = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    val solvedBlock = powScheme
      .proveCandidate(candidateToSolve.candidateBlock, defaultMinerSecret.w, 0, 1000)
      .get

    // Build new transaction to trigger mempool change
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

    // Wait for candidate to expire and trigger forced regeneration
    testProbe.expectNoMessage(200.millis)

    // Request candidate - should be force regenerated due to expiration
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), testProbe.ref)
    val regeneratedCandidate = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(c: Candidate) => c
    }

    // Should be different from the one we're about to solve
    regeneratedCandidate.candidateBlock.transactions.size should be >= candidateToSolve.candidateBlock.transactions.size

    // Submit solution for the old candidate (should still work via cachedPreviousCandidate)
    candidateGenerator.tell(solvedBlock.header.powSolution, testProbe.ref)

    // Should successfully apply the block
    testProbe.fishForMessage(blockValidationDelay) {
      case StatusReply.Success(()) => true
      case FullBlockApplied(header) if header.id != solvedBlock.header.parentId => true
      case _ => false
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

}
