package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.{ErgoAddressEncoder, ErgoScriptPredef}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{ErgoTestHelpers, MempoolTestHelpers, NodeViewTestOps, RandomWrapper}
import org.scalatest.flatspec.AnyFlatSpec
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{LocallyGeneratedTransaction, RecheckedTransactions}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState, FailedTransaction, SuccessfulTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool.ProcessingOutcome
import sigmastate.Values.ErgoTree
import sigmastate.eval.{IRContext, RuntimeIRContext}
import sigmastate.interpreter.Interpreter.emptyEnv

import scala.concurrent.duration._
import scala.util.Random
import sigmastate.lang.Terms.ValueOps
import sigmastate.serialization.ErgoTreeSerializer

class MempoolAuditorSpec extends AnyFlatSpec with NodeViewTestOps with ErgoTestHelpers with MempoolTestHelpers {
  implicit lazy val context: IRContext = new RuntimeIRContext

  val cleanupDuration: FiniteDuration = 200.millis
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(
      mempoolCleanupDuration = cleanupDuration,
      rebroadcastCount = 1
    ))
  val fixture = new NodeViewFixture(settingsToTest, parameters)
  val newTx: Class[SuccessfulTransaction] = classOf[SuccessfulTransaction]

  it should "remove transactions which become invalid" in {
    import fixture._

    val testProbe = new TestProbe(actorSystem)
    actorSystem.eventStream.subscribe(testProbe.ref, newTx)

    val (us, bh) = createUtxoState(parameters)
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis =
      WrappedUtxoState(us, bh, stateConstants, parameters).applyModifier(genesis) { mod =>
        nodeViewHolderRef ! mod
      } .get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootHash)

    val boxes = ErgoState.newBoxes(genesis.transactions).find(_.ergoTree == Constants.TrueLeaf)
    boxes.nonEmpty shouldBe true

    val script = s"{sigmaProp(HEIGHT == ${genesis.height})}"
    val prop = ErgoScriptPredef.compileWithCosting(emptyEnv, script, ErgoAddressEncoder.MainnetNetworkPrefix)
    val tree = ErgoTree.fromProposition(prop.asSigmaProp)

    val bs = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bs) shouldBe tree

    val validTx = validTransactionFromBoxes(boxes.toIndexedSeq, outputsProposition = tree)

    val temporarilyValidTx = validTransactionFromBoxes(validTx.outputs, outputsProposition = proveDlogGen.sample.get)

    subscribeEvents(classOf[FailedTransaction])
    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(validTx, None))
    testProbe.expectMsgClass(cleanupDuration, newTx)
    expectMsgType[ProcessingOutcome.Accepted]

    nodeViewHolderRef ! LocallyGeneratedTransaction(UnconfirmedTransaction(temporarilyValidTx, None))
    testProbe.expectMsgClass(cleanupDuration, newTx)
    expectMsgType[ProcessingOutcome.Accepted]

    getPoolSize shouldBe 2

    val _: ActorRef = MempoolAuditorRef(nodeViewHolderRef, nodeViewHolderRef, settingsToTest)

    Thread.sleep(200) // give transactions in the pool enough time to become candidates for re-checking

    // include first transaction in the block
    val block = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    applyBlock(block) shouldBe 'success

    getPoolSize shouldBe 1 // first tx removed from pool during node view update

    scorex.core.utils.untilTimeout(cleanupDuration * 4, 100.millis) {
      getPoolSize shouldBe 0 // another tx invalidated by `MempoolAuditor`
    }

  }

  it should "rebroadcast transactions correctly" in {

    val (us0, bh0) = createUtxoState(parameters)
    val (txs0, bh1) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)

    val us = us0.applyModifier(b1, None)(_ => ()).get

    val bxs = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    val txs = validTransactionsFromBoxes(200000, bxs, new RandomWrapper)._1
      .map(tx => UnconfirmedTransaction(tx, None))

    implicit val system = ActorSystem()
    val probe = TestProbe()

    val auditor: ActorRef = TestActorRef(new MempoolAuditor(probe.ref, probe.ref, settingsToTest))

    def sendState(): Unit = auditor ! ChangedState(us)
    def sendPool(): Unit = auditor ! ChangedMempool(new FakeMempool(txs))

    val coin = Random.nextBoolean() // flip random coin
    if (coin) {
      sendPool()
      sendState()
    } else {
      sendState()
      sendPool()
    }

    probe.fishForMessage(3.seconds) {
      case _: SendToNetwork => true
      case _: RecheckedTransactions => false
    }.isInstanceOf[SendToNetwork] shouldBe true
  }
}
