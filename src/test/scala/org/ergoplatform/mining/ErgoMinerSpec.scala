package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.{StatusReply, ask}
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.mining.ErgoMiner.StartMining
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ValidBlocksGenerators
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoScriptPredef, Input}
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.FullBlockApplied
import sigmastate.SigmaAnd
import sigmastate.Values.{ErgoTree, SigmaPropConstant}
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.utxo.CostTable

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class ErgoMinerSpec extends AnyFlatSpec with ErgoTestHelpers with ValidBlocksGenerators with Eventually {

  implicit private val timeout: Timeout = defaultTimeout

  private val newBlockSignal: Class[FullBlockApplied] = classOf[FullBlockApplied]
  private val newBlockDelay: FiniteDuration = 30 seconds
  private val candidateGenDelay: FiniteDuration    = 3.seconds
  private val blockValidationDelay: FiniteDuration = 2.seconds

  private def getWorkMessage(minerRef: ActorRef, mandatoryTransactions: Seq[ErgoTransaction]): WorkMessage =
    await(minerRef.askWithStatus(GenerateCandidate(mandatoryTransactions, reply = true)).mapTo[Candidate].map(_.externalVersion))

  val defaultSettings: ErgoSettings = {
    val empty = ErgoSettings.read()

    val nodeSettings = empty.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      internalMinerPollingInterval = 2.second,
      maxTransactionCost = 100000,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = empty.chainSettings.copy(blockInterval = 2.seconds)
    empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
  }

  it should "not include too costly transactions" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)
    val complexScript: ErgoTree = (0 until 100).foldLeft(SigmaAnd(SigmaPropConstant(defaultMinerPk), SigmaPropConstant(defaultMinerPk))) { (l, _) =>
      SigmaAnd(SigmaPropConstant(defaultMinerPk), l)
    }
    complexScript.complexity shouldBe 28077

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMiner(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )
    expectNoMessage(1 second)
    val r: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    minerRef ! StartMining

    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    val boxToSpend: ErgoBox = r.h.bestFullBlockOpt.get.transactions.last.outputs.last
    boxToSpend.propositionBytes shouldBe ErgoScriptPredef.rewardOutputScript(emission.settings.minerRewardDelay, defaultMinerPk).bytes

    val input = Input(boxToSpend.id, emptyProverResult)

    // create transaction with output with costly proposition
    val output = new ErgoBoxCandidate(boxToSpend.value / 10, complexScript, r.s.stateContext.currentHeight)
    val outputs = (0 until 10).map(_ => output)
    val unsignedTx = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs)
    val tx = defaultProver.sign(unsignedTx, IndexedSeq(boxToSpend), IndexedSeq(), r.s.stateContext).get
    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(ErgoTransaction(tx), None))
    expectNoMessage(1 seconds)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 0

    //check that tx is included into UTXO set
    val state = await((readersHolderRef ? GetReaders).mapTo[Readers]).s.asInstanceOf[UtxoState]
    tx.outputs.foreach(o => state.boxById(o.id).get shouldBe o)

    // try to spend all the boxes with complex scripts
    val costlyInputs = tx.outputs.map(o => Input(o.id, emptyProverResult))
    val costlyOut = new ErgoBoxCandidate(tx.outputs.map(_.value).sum, complexScript, r.s.stateContext.currentHeight)
    val unsignedComplexTx = new UnsignedErgoTransaction(costlyInputs, IndexedSeq(), IndexedSeq(costlyOut))
    val costlyTx = defaultProver.sign(unsignedComplexTx, tx.outputs, IndexedSeq(), r.s.stateContext).get

    val txCost =
      state.validateWithCost(
        ErgoTransaction(costlyTx.inputs, costlyTx.dataInputs, costlyTx.outputCandidates),
        Some(r.s.stateContext),
        costLimit = 431780,
        None
      ).get
    txCost shouldBe 431780

    // send costly transaction to the mempool
    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(ErgoTransaction(costlyTx), None))

    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    // costly tx was removed from mempool
    expectNoMessage(1 second)
    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 0
    // costly tx was not included
    val state2 = await((readersHolderRef ? GetReaders).mapTo[Readers]).s.asInstanceOf[UtxoState]
    tx.outputs.foreach(o => state2.boxById(o.id) should not be None)
    costlyTx.outputs.foreach(o => state2.boxById(o.id) shouldBe None)
  }

  it should "not freeze while mempool is full" in new TestKit(ActorSystem()) {
    // generate amount of transactions, twice more than can fit in one block
    val desiredSize: Int = Math.ceil((parameters.maxBlockCost / CostTable.interpreterInitCost) * 1.2).toInt
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    expectNoMessage(1 second)
    val r: Readers = requestReaders
    val wallet: ErgoWalletReader = r.w

    val minerRef: ActorRef = ErgoMiner(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )
    minerRef ! StartMining

    // wait for 1 block to be generated
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    @tailrec
    def loop(toSend: Int): Unit = {
      val toSpend: Seq[ErgoBox] = await(
        wallet.walletBoxes(unspentOnly = false, considerUnconfirmed = false)
      ).map(_.trackedBox.box).toList
      log.debug(s"Generate more transactions from ${toSpend.length} boxes. $toSend remains," +
        s"pool size: ${requestReaders.m.size}")
      val txs: Seq[ErgoTransaction] = toSpend.take(toSend) map { boxToSend =>
        val inputs = IndexedSeq(Input(boxToSend.id, emptyProverResult))

        val feeBox = new ErgoBoxCandidate(boxToSend.value / desiredSize, feeProp, r.s.stateContext.currentHeight)
        val outputs = (1 until desiredSize).map { _ =>
          new ErgoBoxCandidate(boxToSend.value / desiredSize, defaultMinerPk, r.s.stateContext.currentHeight)
        }
        val unsignedTx = new UnsignedErgoTransaction(inputs, IndexedSeq(), feeBox +: outputs)
        ErgoTransaction(
          defaultProver.sign(
            unsignedTx,
            IndexedSeq(boxToSend),
            IndexedSeq(),
            r.s.stateContext
          ).get
        )
      }

      txs.map(tx => UnconfirmedTransaction(tx, None)).foreach(nodeViewHolderRef ! LocallyGeneratedTransaction(_))

      if (toSend > toSpend.size) {
        // wait for the next block
        testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
        loop(toSend - toSpend.size)
      }
    }

    def requestReaders: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    // Generate and send `desiredSize` transactions to mempool
    loop(desiredSize)

    implicit val patienceConfig: PatienceConfig = PatienceConfig(10.second, 100.millis)
    eventually {
      requestReaders.m.size should be > 10
    }

    // wait for mempool to be cleaned
    scorex.core.utils.untilTimeout(5.minute, 500.millis) {
      log.debug(s"Wait until transactions in mempool will be included into blocks. Currents size: ${requestReaders.m.size}")
      requestReaders.m.size shouldBe 0
      system.terminate()
    }
  }

  it should "include only one transaction from 2 spending the same box" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMiner(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )
    expectNoMessage(1 second)
    val r: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    val history: ErgoHistoryReader = r.h
    val startBlock: Option[Header] = history.bestHeaderOpt

    minerRef ! StartMining

    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    val prop1: DLogProtocol.ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray("test1".getBytes())).publicImage
    val prop2: DLogProtocol.ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray("test2".getBytes())).publicImage

    val boxToDoubleSpend: ErgoBox = r.h.bestFullBlockOpt.get.transactions.last.outputs.last
    boxToDoubleSpend.propositionBytes shouldBe ErgoScriptPredef.rewardOutputScript(emission.settings.minerRewardDelay, defaultMinerPk).bytes

    val input = Input(boxToDoubleSpend.id, emptyProverResult)

    val outputs1 = IndexedSeq(new ErgoBoxCandidate(boxToDoubleSpend.value, prop1, r.s.stateContext.currentHeight))
    val unsignedTx1 = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs1)
    val tx1 = defaultProver.sign(unsignedTx1, IndexedSeq(boxToDoubleSpend), IndexedSeq(), r.s.stateContext).get
    val outputs2 = IndexedSeq(new ErgoBoxCandidate(boxToDoubleSpend.value, prop2, r.s.stateContext.currentHeight))
    val unsignedTx2 = new UnsignedErgoTransaction(IndexedSeq(input), IndexedSeq(), outputs2)
    val tx2 = ErgoTransaction(defaultProver.sign(unsignedTx2, IndexedSeq(boxToDoubleSpend), IndexedSeq(), r.s.stateContext).get)

    // As double-spending transactions are filtered out in the mempool, the only way to push them is to order to
    // include double-spending transaction directly via mandatoryTransactions argument of PrepareCandidate command
    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(ErgoTransaction(tx1), None))
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    testProbe.expectNoMessage(200.millis)
    minerRef.tell(GenerateCandidate(Seq(tx2), reply = true), testProbe.ref)
    testProbe.expectMsgPF(candidateGenDelay) {
      case StatusReply.Success(candidate: Candidate) =>
        val block = defaultSettings.chainSettings.powScheme
          .proveCandidate(candidate.candidateBlock, defaultMinerSecret.w, 0, 1000)
          .get
        testProbe.expectNoMessage(200.millis)
        minerRef.tell(block.header.powSolution, testProbe.ref)

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
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 0

    val blocks: IndexedSeq[ErgoFullBlock] = r.h.chainToHeader(startBlock, r.h.bestHeaderOpt.get)._2.headers.flatMap(r.h.getFullBlock)
    val txs: Seq[ErgoTransaction] = blocks.flatMap(_.blockTransactions.transactions)
    //Make sure that only tx got into chain
    txs.filter(tx => tx.id == tx1.id || tx.id == tx2.id) should have length 1
    system.terminate()
  }

  it should "prepare external candidate" in new TestKit(ActorSystem()) {
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    def minerRef: ActorRef = ErgoMiner(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )

    val passiveMiner: ActorRef = minerRef
    passiveMiner ! StartMining

    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 200.millis) // it takes a while before PK is set
    eventually(await(passiveMiner.askWithStatus(GenerateCandidate(Seq.empty, reply = true)).mapTo[Candidate]))
    system.terminate()
  }

  it should "include mandatory transactions" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMiner(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )
    expectNoMessage(1 second)
    val r: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    minerRef ! StartMining

    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    val prop1: DLogProtocol.ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray("test1".getBytes())).publicImage
    val prop2: DLogProtocol.ProveDlog = DLogProverInput(BigIntegers.fromUnsignedByteArray("test2".getBytes())).publicImage

    val mBox: ErgoBox = r.h.bestFullBlockOpt.get.transactions.last.outputs.last
    val mInput = Input(mBox.id, emptyProverResult)

    val outputs1 = IndexedSeq(new ErgoBoxCandidate(mBox.value, prop1, r.s.stateContext.currentHeight))
    val unsignedTx1 = new UnsignedErgoTransaction(IndexedSeq(mInput), IndexedSeq(), outputs1)
    val mandatoryTxLike1 = defaultProver.sign(unsignedTx1, IndexedSeq(mBox), IndexedSeq(), r.s.stateContext).get
    val mandatoryTx1 = ErgoTransaction(mandatoryTxLike1)

    val outputs2 = IndexedSeq(new ErgoBoxCandidate(mBox.value, prop2, r.s.stateContext.currentHeight))
    val unsignedTx2 = new UnsignedErgoTransaction(IndexedSeq(mInput), IndexedSeq(), outputs2)
    val mandatoryTxLike2 = defaultProver.sign(unsignedTx2, IndexedSeq(mBox), IndexedSeq(), r.s.stateContext).get
    val mandatoryTx2 = ErgoTransaction(mandatoryTxLike2)
    mandatoryTx1.bytes.sameElements(mandatoryTx2.bytes) shouldBe false

    val ecb = getWorkMessage(minerRef, Seq.empty)
    ecb.proofsForMandatoryTransactions.isDefined shouldBe false

    val ecb1 = getWorkMessage(minerRef, Seq(mandatoryTx1))
    ecb1.proofsForMandatoryTransactions.get.txProofs.length shouldBe 1
    ecb1.proofsForMandatoryTransactions.get.check() shouldBe true

    val ecb2 = getWorkMessage(minerRef, Seq(mandatoryTx2))
    ecb2.msg.sameElements(ecb1.msg) shouldBe false
    ecb2.proofsForMandatoryTransactions.get.txProofs.length shouldBe 1
    ecb2.proofsForMandatoryTransactions.get.check() shouldBe true

    val ecb3 = getWorkMessage(minerRef, Seq.empty)
    ecb3.msg.sameElements(ecb2.msg) shouldBe true
    ecb3.proofsForMandatoryTransactions.get.txProofs.length shouldBe 1
    ecb3.proofsForMandatoryTransactions.get.check() shouldBe true

    system.terminate()
  }

  it should "mine after HF" in new TestKit(ActorSystem()) {
    val forkHeight = 3

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlockSignal)

    val forkSettings: ErgoSettings = {
      val empty = ErgoSettings.read()

      val nodeSettings = empty.nodeSettings.copy(mining = true,
        stateType = StateType.Utxo,
        internalMinerPollingInterval = 2.second,
        offlineGeneration = true,
        verifyTransactions = true)
      val chainSettings = empty.chainSettings.copy(
        blockInterval = 2.seconds,
        epochLength = forkHeight,
        voting = empty.chainSettings.voting.copy(
          version2ActivationHeight = forkHeight,
          version2ActivationDifficultyHex = "10",
          votingLength = forkHeight)
      )
      empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings, directory = createTempDir.getAbsolutePath)
    }

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(forkSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMiner(
      forkSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )

    minerRef ! StartMining

    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

    val wm1 = getWorkMessage(minerRef, Seq.empty)
    (wm1.h.get >= forkHeight) shouldBe true

    testProbe.expectMsgClass(newBlockDelay, newBlockSignal)
    implicit val patienceConfig: PatienceConfig = PatienceConfig(1.seconds, 50.millis)
    eventually {
      val wm2 = getWorkMessage(minerRef, Seq.empty)
      (wm2.h.get >= forkHeight) shouldBe true
      wm1.msg.sameElements(wm2.msg) shouldBe false

      val v2Block = testProbe.expectMsgClass(newBlockDelay, newBlockSignal)

      val h2 = v2Block.header
      h2.version shouldBe 2
      h2.minerPk shouldBe defaultMinerPk.value
    }
  }

}
