package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{TestKit, TestProbe}
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.{ErgoMiner, ErgoMinerRef}
import org.ergoplatform.mining.Listener._
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.{ErgoTestHelpers, ValidBlocksGenerators}
import org.ergoplatform.{ErgoBoxCandidate, Input}
import org.scalatest.FlatSpec
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SuccessfulTransaction}
import scorex.util.ModifierId
import sigmastate.SBoolean
import sigmastate.Values.{TrueLeaf, Value}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.utxo.CostTable.Cost

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class ErgoMinerSpec extends FlatSpec with ErgoTestHelpers with ValidBlocksGenerators {

  type msgType = SemanticallySuccessfulModifier[_]
  val defaultAwaitDuration: FiniteDuration = 10.seconds
  implicit val timeout: Timeout = Timeout(defaultAwaitDuration)
  val newBlock: Class[msgType] = classOf[msgType]
  val newBlockDuration = 30 seconds

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


  def await[A](f: Future[A]): A = Await.result[A](f, defaultAwaitDuration)

  it should "not freeze while mempool is full" in new TestKit(ActorSystem()) {
    // generate amount of transactions, twice more than can fit in one block
    val desiredSize: Int = ((Constants.MaxBlockCost / Cost.Dlog) * 2).toInt
    val outputsPerTx = desiredSize
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)
    private val prover = new ErgoProvingInterpreter("test1", settings.walletSettings.dlogSecretsNumber)
    val prop: Value[SBoolean.type] = prover.dlogPubkeys.head

    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlock)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)
    expectNoMessage(1 second)
    val r: Readers = await((readersHolderRef ? GetReaders).mapTo[Readers])
    val pool: ErgoMemPoolReader = r.m
    val wallet: ErgoWalletReader = r.w
    val address: P2PKAddress = await(wallet.randomPublicKey())

    val minerRef: ActorRef = ErgoMinerRef(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      emission,
      Some(prop)
    )
    minerRef ! StartMining

    // wait for 1 block to be generated
    testProbe.expectMsgClass(newBlockDuration, newBlock)

    // wait for mempool to contain 1000 transactions
    def loop(toSend: Int): Unit = {
      val toSpend = r.h.bestFullBlockOpt.get.transactions.flatMap(_.outputs).filter(_.proposition == prop)
      log.debug(s"Generate more transactions from ${toSpend.length} boxes. $toSend remains, pool size: ${pool.size}")
      toSpend.take(toSend) foreach { boxToSend =>
        val inputs = IndexedSeq(Input(boxToSend.id, ProverResult(Array.emptyByteArray, ContextExtension.empty)))

        val outputs = (0 until outputsPerTx).map(_ => new ErgoBoxCandidate(boxToSend.value / outputsPerTx, prop))

        val unsignedTx = new UnsignedErgoTransaction(inputs, outputs)
        val tx = prover.sign(unsignedTx, IndexedSeq(boxToSend), ErgoStateContext(r.h.fullBlockHeight, r.s.rootHash)).get
        nodeViewHolderRef ! LocallyGeneratedTransaction(tx)
      }
      if (toSend > toSpend.size) {
        Thread.sleep(1000)
        loop(toSend - toSpend.size)
      }
    }

    loop(desiredSize)

    // wait for mempool to be cleaned
    while (pool.size > 0) {
      log.debug(s"Wait until transactions in mempool will be included into blocks. Currents size: ${pool.size}")
      // blocks should not be empty
      r.h.bestFullBlockOpt.get.transactions.nonEmpty shouldBe true
      Thread.sleep(1000)
    }

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

  it should "create own coinbase transaction, if there is already a transaction, that spends emission box" in
    new TestKit(ActorSystem()) {
      val tmpDir = createTempDir

      val testProbe = new TestProbe(system)
      system.eventStream.subscribe(testProbe.ref, classOf[SemanticallySuccessfulModifier[_]])
      system.eventStream.subscribe(testProbe.ref, classOf[SuccessfulTransaction[_]])

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

      val minerRef: ActorRef = ErgoMinerRef(
        ergoSettings,
        nodeViewHolderRef,
        readersHolderRef,
        timeProvider,
        emission,
        Some(DLogProverInput(BigIntegers.fromUnsignedByteArray("test".getBytes())).publicImage))
      expectNoMessage(1 second)
      val r: Readers = Await.result((readersHolderRef ? GetReaders).mapTo[Readers], 10 seconds)

      val prop1 = DLogProverInput(BigIntegers.fromUnsignedByteArray("test1".getBytes())).publicImage

      val tx = ErgoMiner.createCoinbase(r.s.asInstanceOf[UtxoStateReader], Seq.empty, prop1, emission)
      r.s.asInstanceOf[UtxoStateReader].validate(tx) shouldBe 'success

      nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx)
      testProbe.expectMsgType[SuccessfulTransaction[ErgoTransaction]]
      r.m.size shouldBe 1

      minerRef ! StartMining

      testProbe.expectMsgClass(newBlockDuration, classOf[SemanticallySuccessfulModifier[_]])

      val blocks = r.h.chainToHeader(None, r.h.bestHeaderOpt.get)._2.headers.flatMap(r.h.getFullBlock)
      val txIds: Seq[ModifierId] = blocks.flatMap(_.blockTransactions.txs.map(_.id))
      //Make sure that this tx wasn't mined
      txIds.contains(tx.id) shouldBe false
      system.terminate()
    }

  it should "include only one transaction from 2 spending the same box" in new TestKit(ActorSystem()) {
    val testProbe = new TestProbe(system)
    system.eventStream.subscribe(testProbe.ref, newBlock)
    val ergoSettings: ErgoSettings = defaultSettings.copy(directory = createTempDir.getAbsolutePath)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

    val minerRef: ActorRef = ErgoMinerRef(
      ergoSettings,
      nodeViewHolderRef,
      readersHolderRef,
      timeProvider,
      emission,
      Some(TrueLeaf)
    )
    expectNoMessage(1 second)
    val r: Readers = Await.result((readersHolderRef ? GetReaders).mapTo[Readers], 10 seconds)

    val history: ErgoHistoryReader = r.h
    val startBlock = history.bestHeaderOpt

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
    val txs: Seq[ErgoTransaction] = blocks.flatMap(_.blockTransactions.transactions)
    //Make sure that only tx got into chain
    txs.filter(tx => tx.id == tx1.id || tx.id == tx2.id) should have length 1
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
