package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.{StatusReply, ask}
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction, UnsignedErgoTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.FullBlockApplied
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.NetworkType.DevNet60
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoTestHelpers
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

  val defaultSettings60 = defaultSettings.copy(networkType = DevNet60, directory = defaultSettings.directory + "60")

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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)

    val block = testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
    }

    // now block should be cached
    (0 to 20).foreach { _ =>
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        // this triggers mempool change that triggers candidate regeneration
        viewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(tx, None))
        expectNoMessage(candidateGenDelay)
        candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
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
    candidateGenerator.tell(GenerateCandidate(Seq(tx), reply = true), testProbe.ref)
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

  it should "6.0 pool transactions should be removed from pool when 5.0 block is mined" in new TestKit(
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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
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

    val outputs = IndexedSeq(
      new ErgoBoxCandidate(rewardBox.value, TrueTree, readers.s.stateContext.currentHeight)
    )
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)

    val tx = ErgoTransaction(
      defaultProver
        .sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext)
        .get
    )

    // sigmaProp(Global.serialize(2).size > 0)
    val bs = "1b110204040400d191b1dc6a03dd0173007301"
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get)

    val spendingBox = tx.outputs.head
    val o2 = new ErgoBoxCandidate(spendingBox.value, tree, spendingBox.creationHeight, spendingBox.additionalTokens, spendingBox.additionalRegisters)
    val tx2 = tx.copy(
      inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o2))

    testProbe.expectNoMessage(200.millis)
    // mine a block with that transaction
    candidateGenerator.tell(GenerateCandidate(Seq(tx, tx2), reply = true), testProbe.ref)
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
    txs should have length 2 // 1 rewards and one regular tx, no fee collection
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
    candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true), testProbe.ref)
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

    val outputs = IndexedSeq(
      new ErgoBoxCandidate(rewardBox.value, TrueTree, readers.s.stateContext.currentHeight)
    )
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)

    val tx = ErgoTransaction(
      defaultProver
        .sign(unsignedTx, IndexedSeq(rewardBox), IndexedSeq(), readers.s.stateContext)
        .get
    )

    // sigmaProp(Global.serialize(2).size > 0)
    val bs = "1b110204040400d191b1dc6a03dd0173007301"
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(bs).get)

    val spendingBox = tx.outputs.head
    val o2 = new ErgoBoxCandidate(spendingBox.value, tree, spendingBox.creationHeight, spendingBox.additionalTokens, spendingBox.additionalRegisters)
    val tx2 = tx.copy(
      inputs = IndexedSeq(new Input(spendingBox.id, emptyProverResult)),
      outputCandidates = IndexedSeq(o2))

    testProbe.expectNoMessage(200.millis)
    // mine a block with that transaction
    candidateGenerator.tell(GenerateCandidate(Seq(tx, tx2), reply = true), testProbe.ref)
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

}
