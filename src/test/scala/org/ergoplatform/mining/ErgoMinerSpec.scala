package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse, StartMining}
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoMiner, ErgoMinerRef, TransactionGeneratorRef}
import org.ergoplatform.mining.Listener._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.{StateType, UtxoStateReader}
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, TestingSettings}
import org.ergoplatform.utils.{ErgoTestHelpers, ValidBlocksGenerators}
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalatest.FlatSpec
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.ModifierId
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import sigmastate.Values.TrueLeaf
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class ErgoMinerSpec extends FlatSpec with ErgoTestHelpers with ValidBlocksGenerators {

  val defaultAwaitDuration: FiniteDuration = 5.seconds
  implicit val timeout: Timeout = Timeout(defaultAwaitDuration)

  def await[A](f: Future[A]): A = Await.result[A](f, defaultAwaitDuration)

  it should "not freeze while generating candidate block with large amount of txs" in new TestKit(ActorSystem()){
    val tmpDir = createTempDir

    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = tmpDir.getAbsolutePath)

    val nodeSettings = defaultSettings.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      miningDelay = defaultAwaitDuration,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = defaultSettings.chainSettings.copy(blockInterval = 2.seconds)
    val ergoSettings = defaultSettings.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider, emission)
    val listener = system.actorOf(Props(new Listener))

    val testingSettings = TestingSettings(transactionGeneration = true, 500)
    val txGen = TransactionGeneratorRef(nodeViewHolderRef, testingSettings)
    txGen ! StartGeneration

    minerRef ! StartMining
    expectNoMessage(20.seconds)

    //check that miner actor is still alive
    noException should be thrownBy {
      val result = await((minerRef ? MiningStatusRequest).mapTo[MiningStatusResponse])
      result.isMining shouldBe true
      result.candidateBlock shouldBe defined
      val height = result.candidateBlock.get.parentOpt.get.height
      val blocksGenerated = await((listener ? Status).mapTo[Int])
      blocksGenerated should be > 1
      blocksGenerated should be >= height
    }

    system.terminate()
  }

  it should "filter out double spend txs" in {
    val tx = validErgoTransactionGen.sample.get._2
    ErgoMiner.fixTxsConflicts(Seq(tx, tx, tx)) should have length 1

    val inputs = validErgoTransactionGenTemplate(0, -1, 100, 100).sample.get._1
    val (l, r) = inputs.splitAt(50)
    val tx_1 = validTransactionGen(l).sample.get
    val tx_2 = validTransactionGen(r :+ l.last).sample.get

    ErgoMiner.fixTxsConflicts(Seq(tx_1, tx_2, tx)) should contain theSameElementsAs Seq(tx_1, tx)
    ErgoMiner.fixTxsConflicts(Seq(tx_2, tx_1, tx)) should contain theSameElementsAs Seq(tx_2, tx)
  }

  it should "create own coinbase transaction, if there is already a transaction, that spends emission box" in new TestKit(ActorSystem()){
    val tmpDir = createTempDir

    type msgType = SemanticallySuccessfulModifier[_]
    val newBlock = classOf[msgType]
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlock)
    val newBlockDuration = 30 seconds

    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = tmpDir.getAbsolutePath)

    val nodeSettings = defaultSettings.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      miningDelay = defaultAwaitDuration,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = defaultSettings.chainSettings.copy(blockInterval = 2.seconds)
    val ergoSettings = defaultSettings.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider, emission, TrueLeaf)
    expectNoMessage(1 second)
    val r: Readers = Await.result((readersHolderRef ? GetReaders).mapTo[Readers], 10 seconds)

    val emissionBox = r.s.asInstanceOf[UtxoStateReader].emissionBoxOpt.get

    val prop1 = DLogProverInput(BigIntegers.fromUnsignedByteArray("test1".getBytes())).publicImage

    val input = Input(emissionBox.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))
    val outputs = IndexedSeq(new ErgoBoxCandidate(1000000L, prop1))
    val tx = new ErgoTransaction(IndexedSeq(input), outputs)

    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)

    minerRef ! StartMining

    testProbe.expectMsgClass(newBlockDuration, newBlock)

    r.m.unconfirmed.size shouldBe 0

    val blocks = r.h.chainToHeader(None, r.h.bestHeaderOpt.get)._2.headers.flatMap(r.h.getFullBlock)
    val txIds: Seq[ModifierId]  = blocks.flatMap(_.blockTransactions.txs.map(_.id))
    //Make sure that this tx wasn't mined
    txIds.contains(tx.id) shouldBe false
    system.terminate()
  }

  it should "work correctly with 2 coinbase txs in pool" in new TestKit(ActorSystem()){
    val tmpDir = createTempDir

    type msgType = SemanticallySuccessfulModifier[_]
    val newBlock = classOf[msgType]
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlock)

    val defaultSettings: ErgoSettings = ErgoSettings.read(None).copy(directory = tmpDir.getAbsolutePath)

    val nodeSettings = defaultSettings.nodeSettings.copy(mining = true,
      stateType = StateType.Utxo,
      miningDelay = defaultAwaitDuration,
      offlineGeneration = true,
      verifyTransactions = true)
    val chainSettings = defaultSettings.chainSettings.copy(blockInterval = 2.seconds)
    val ergoSettings = defaultSettings.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider, emission, TrueLeaf)
    expectNoMessage(1 second)
    val r: Readers = Await.result((readersHolderRef ? GetReaders).mapTo[Readers], 10 seconds)

    val history: ErgoHistoryReader = r.h
    val startBlock = history.bestHeaderOpt
    val newBlockDuration = 30 seconds

    minerRef ! StartMining

    testProbe.expectMsgClass(newBlockDuration, newBlock)

    val prop1 = DLogProverInput(BigIntegers.fromUnsignedByteArray("test1".getBytes())).publicImage
    val prop2 = DLogProverInput(BigIntegers.fromUnsignedByteArray("test2".getBytes())).publicImage

    val boxToDoubleSpend = r.h.bestFullBlockOpt.get.transactions.last.outputs.last
    val input = Input(boxToDoubleSpend.id, ProverResult(Array.emptyByteArray, ContextExtension.empty))

    val outputs1 = IndexedSeq(new ErgoBoxCandidate(boxToDoubleSpend.value, prop1))
    val tx1 = new ErgoTransaction(IndexedSeq(input), outputs1)

    val outputs2 = IndexedSeq(new ErgoBoxCandidate(boxToDoubleSpend.value, prop2))
    val tx2 = new ErgoTransaction(IndexedSeq(input), outputs2)

    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx1)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx2)
    expectNoMessage(1 seconds)

    r.m.unconfirmed.size shouldBe 2

    testProbe.expectMsgClass(newBlockDuration, newBlock)
    testProbe.expectMsgClass(newBlockDuration, newBlock)
    testProbe.expectMsgClass(newBlockDuration, newBlock)

    r.m.unconfirmed.size shouldBe 0

    val blocks = r.h.chainToHeader(startBlock, r.h.bestHeaderOpt.get)._2.headers.flatMap(r.h.getFullBlock)
    val txs: Seq[ErgoTransaction]  = blocks.flatMap(_.blockTransactions.transactions)
    //Make sure that only tx got into chain
    txs.filter(tx => tx.id.sameElements(tx1.id) || tx.id.sameElements(tx2.id)) should have length 1
    system.terminate()
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
