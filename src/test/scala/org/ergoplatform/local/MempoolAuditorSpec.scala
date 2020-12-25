package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.{ErgoAddressEncoder, ErgoScriptPredef}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.{ErgoMemPoolReader, OrderedTxPool}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Constants, Algos, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{NodeViewTestOps, ErgoTestHelpers}
import org.scalatest.flatspec.AnyFlatSpec
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SuccessfulTransaction, FailedTransaction, ChangedState, ChangedMempool}
import scorex.util.ModifierId
import sigmastate.Values.ErgoTree
import sigmastate.eval.{RuntimeIRContext, IRContext}
import sigmastate.interpreter.Interpreter.emptyEnv

import scala.concurrent.duration._
import scala.util.Random
import sigmastate.lang.Terms.ValueOps
import sigmastate.serialization.ErgoTreeSerializer

class MempoolAuditorSpec extends AnyFlatSpec with NodeViewTestOps with ErgoTestHelpers {
  implicit lazy val context: IRContext = new RuntimeIRContext

  val cleanupDuration: FiniteDuration = 2.seconds
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(
      mempoolCleanupDuration = cleanupDuration,
      rebroadcastCount = 1
    ))
  val fixture = new NodeViewFixture(settingsToTest)
  val newTx: Class[SuccessfulTransaction[_]] = classOf[SuccessfulTransaction[_]]

  it should "expose transactions which become invalid" in {
    import fixture._

    val testProbe = new TestProbe(actorSystem)
    actorSystem.eventStream.subscribe(testProbe.ref, newTx)

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootHash)

    val boxes = ErgoState.boxChanges(genesis.transactions)._2.find(_.ergoTree == Constants.TrueLeaf)
    boxes.nonEmpty shouldBe true

    val script = s"{sigmaProp(HEIGHT == ${genesis.height})}"
    val prop = ErgoScriptPredef.compileWithCosting(emptyEnv, script, ErgoAddressEncoder.MainnetNetworkPrefix)
    val tree = ErgoTree.fromProposition(prop.asSigmaProp)

    val bs = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bs) shouldBe tree

    val validTx = validTransactionFromBoxes(boxes.toIndexedSeq, outputsProposition = tree)

    val temporarilyValidTx = validTransactionFromBoxes(validTx.outputs, outputsProposition = proveDlogGen.sample.get)

    subscribeEvents(classOf[FailedTransaction])
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](validTx)
    testProbe.expectMsgClass(cleanupDuration, newTx)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](temporarilyValidTx)
    testProbe.expectMsgClass(cleanupDuration, newTx)
    getPoolSize shouldBe 2

    val _: ActorRef = MempoolAuditorRef(nodeViewHolderRef, nodeViewHolderRef, settingsToTest)

    // include first transaction in the block
    val block = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    applyBlock(block) shouldBe 'success

    getPoolSize shouldBe 1 // first tx removed from pool during node view update

    scorex.core.utils.untilTimeout(cleanupDuration * 2, 100.millis) {
      getPoolSize shouldBe 0 // another tx invalidated by `MempoolAuditor`
    }

  }

  it should "rebroadcast transactions correctly" in {

    val (us0, bh0) = createUtxoState(None)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)

    val us = us0.applyModifier(b1).get

    val bxs = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val txs = validTransactionsFromBoxes(200000, bxs, new Random())._1

    // mempool reader stub specifically for this test
    // only take is defined as only this method is used in rebroadcasting
    object fakeMempool extends ErgoMemPoolReader {
      override type NVCT = this.type

      override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = ???

      override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ???

      override def size: Int = ???

      override def weightedTransactionIds(limit: Int): Seq[OrderedTxPool.WeightedTxId] = ???

      override def getAll: Seq[ErgoTransaction] = ???

      override def getAllPrioritized: Seq[ErgoTransaction] = txs

      override def take(limit: Int): Iterable[ErgoTransaction] = txs.take(limit)

      override def spentInputs: Iterator[BoxId] = txs.flatMap(_.inputs).map(_.boxId).toIterator

      override def getRecommendedFee(expectedWaitTimeMinutes : Int, txSize : Int) : Long = 0

      override def getExpectedWaitTime(txFee : Long, txSize : Int): Long = 0

    }

    implicit val system = ActorSystem()
    val probe = TestProbe()

    val auditor: ActorRef = TestActorRef(new MempoolAuditor(probe.ref, probe.ref, settingsToTest))

    val coin = Random.nextBoolean()

    def sendState(): Unit = auditor ! ChangedState(us)
    def sendPool(): Unit = auditor ! ChangedMempool(fakeMempool)

    if (coin) {
      sendPool()
      sendState()
    } else {
      sendState()
      sendPool()
    }

    probe.expectMsgType[SendToNetwork]
  }
}
