package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill}
import akka.pattern.ask
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.local.ErgoMiner.{PrepareCandidate, StartMining}
import org.ergoplatform.local.ErgoMinerRef
import org.ergoplatform.mining.Listener._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.utils.generators.ValidBlocksGenerators
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoScriptPredef, Input}
import org.scalatest.FlatSpec
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import sigmastate.basics.DLogProtocol
import sigmastate.basics.DLogProtocol.DLogProverInput
import sigmastate.utxo.CostTable.Cost

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps

class ErgoMinerSpec extends FlatSpec with ErgoTestHelpers with ValidBlocksGenerators {

  type msgType = SemanticallySuccessfulModifier[_]
  implicit private val timeout: Timeout = defaultTimeout

  val newBlock: Class[msgType] = classOf[msgType]
  val newBlockDuration: FiniteDuration = 30 seconds

  val defaultSettings: ErgoSettings = {
    val empty = ErgoSettings.read(None)

    val nodeSettings = empty.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      miningDelay = 2.second,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = empty.chainSettings.copy(blockInterval = 2.seconds)
    empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
  }

  it should "not freeze while mempool is full" in new TestKit(ActorSystem()) {
    // generate amount of transactions, twice more than can fit in one block
    val desiredSize: Int = ((parameters.maxBlockCost / Cost.DlogDeclaration) * 2).toInt
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlock)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    expectNoMessage(1 second)
    val r: Readers = requestReaders
    val pool: ErgoMemPoolReader = r.m
    val wallet: ErgoWalletReader = r.w

    val minerRef: ActorRef = ErgoMinerRef(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )
    minerRef ! StartMining

    // wait for 1 block to be generated
    testProbe.expectMsgClass(newBlockDuration, newBlock)

    @tailrec
    def loop(toSend: Int): Unit = {
      val toSpend: Seq[ErgoBox] = await(wallet.unspendBoxes()).toList
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

      txs.foreach(nodeViewHolderRef ! LocallyGeneratedTransaction(_))

      if (toSend > toSpend.size) {
        // wait for the next block
        testProbe.expectMsgClass(newBlockDuration, newBlock)
        loop(toSend - toSpend.size)
      }
    }

    def requestReaders: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])

    // Generate and send `desiredSize` transactions to mempool
    loop(desiredSize)

    Thread.sleep(5000)

    requestReaders.m.size should be > 10

    // wait for mempool to be cleaned
    scorex.core.utils.untilTimeout(5.minute, 500.millis) {
      log.debug(s"Wait until transactions in mempool will be included into blocks. Currents size: ${requestReaders.m.size}")
      requestReaders.m.size shouldBe 0
    }
  }

  it should "include only one transaction from 2 spending the same box" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlock)
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMinerRef(
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

    testProbe.expectMsgClass(newBlockDuration, newBlock)

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
    val tx2 = defaultProver.sign(unsignedTx2, IndexedSeq(boxToDoubleSpend), IndexedSeq(), r.s.stateContext).get

    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](ErgoTransaction(tx1))
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](ErgoTransaction(tx2))
    expectNoMessage(1 seconds)

    await((readersHolderRef ? GetReaders).mapTo[Readers]).m.size shouldBe 2

    testProbe.expectMsgClass(newBlockDuration, newBlock)
    testProbe.expectMsgClass(newBlockDuration, newBlock)
    testProbe.expectMsgClass(newBlockDuration, newBlock)

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

    def minerRef: ActorRef = ErgoMinerRef(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      Some(defaultMinerSecret)
    )

    val passiveMiner: ActorRef = minerRef

    await((passiveMiner ? PrepareCandidate).mapTo[Future[ExternalCandidateBlock]].flatten)
  }

}

class Listener extends Actor {
  var generatedBlocks: Int = 0

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(_) => generatedBlocks += 1
    case Status => sender ! generatedBlocks
  }
}

object Listener {

  case object Status

}
