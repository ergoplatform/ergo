package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.TestKit
import akka.util.Timeout
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse, StartMining}
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoMiner, ErgoMinerRef, TransactionGeneratorRef}
import org.ergoplatform.mining.Listener._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, TestingSettings}
import org.ergoplatform.utils.ErgoTestHelpers
import org.scalatest.FlatSpecLike
import scapi.sigma.DLogProtocol.DLogProverInput
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedTransaction}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class ErgoMinerSpec extends TestKit(ActorSystem()) with FlatSpecLike with ErgoTestHelpers {

  val defaultAwaitDuration: FiniteDuration = 5.seconds
  implicit val timeout: Timeout = Timeout(defaultAwaitDuration)

  def await[A](f: Future[A]): A = Await.result[A](f, defaultAwaitDuration)

  it should "not freeze while generating candidate block with large amount of txs" in {
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

  it should "not add double spend txs to pool" in {
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

    val state: UtxoState = await(
      (nodeViewHolderRef ? GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, UtxoState]
        (v => v.state)).mapTo[UtxoState]
    )

    val minerProp = DLogProverInput(BigIntegers
      .fromUnsignedByteArray(ergoSettings.scorexSettings.network.nodeName.getBytes())
    ).publicImage

    val fakeProp = DLogProverInput(BigIntegers.fromUnsignedByteArray("hey hey hey".getBytes())).publicImage

    val emissionBox = state.emissionBoxOpt.get
    val tx_1 = ErgoMiner.createCoinbase(emissionBox, state.stateContext.height, Seq.empty, minerProp, emission)
    val tx_1_1 = ErgoMiner.createCoinbase(emissionBox, state.stateContext.height, Seq.empty, fakeProp, emission)
    val oCandidates = tx_1.outputCandidates
    val c1 = oCandidates.head
    val c2 = oCandidates.drop(1).head

    val newCandidates = IndexedSeq(
      new ErgoBoxCandidate(c1.value - 1L, c1.proposition, c1.additionalTokens, c1.additionalRegisters),
      new ErgoBoxCandidate(c2.value + 1L, c2.proposition, c2.additionalTokens, c2.additionalRegisters)
    )

    val tx_2 = tx_1.copy(outputCandidates = newCandidates)

    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx_1)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx_2)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx_1)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx_1_1)
    expectNoMessage(1 seconds)

    val unconfirmedSize = await(
      (nodeViewHolderRef ? GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int]
        {v =>
          v.pool.unconfirmed.values.size
        }).mapTo[Int]
    )

    unconfirmedSize shouldBe 2

    minerRef ! StartMining

    //wait for block to be mined
    expectNoMessage(9 seconds)

    val unconfirmedSize2 = await(
      (nodeViewHolderRef ? GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Int]
        {v =>
          v.pool.unconfirmed.values.size
        }).mapTo[Int]
    )

    //mempool should eliminate double spent tx in case if first spent heppened
    unconfirmedSize2 shouldBe 0
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
