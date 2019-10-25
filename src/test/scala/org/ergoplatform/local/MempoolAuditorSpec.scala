package org.ergoplatform.local

import akka.actor.ActorRef
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{ErgoTestHelpers, NodeViewTestOps}
import org.scalatest.FlatSpec
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{FailedTransaction, SuccessfulTransaction}

import scala.concurrent.duration._

class MempoolAuditorSpec extends FlatSpec with NodeViewTestOps with ErgoTestHelpers {

  val cleanupDuration: FiniteDuration = 5.seconds
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(mempoolCleanupDuration = cleanupDuration))
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

    val validTx = validTransactionFromBoxes(boxes.toIndexedSeq)
    val doubleSpendTx = validTransactionFromBoxes(boxes.toIndexedSeq, outputsProposition = proveDlogGen.sample.get)

    subscribeEvents(classOf[FailedTransaction])
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](validTx)
    testProbe.expectMsgClass(cleanupDuration, newTx)
    nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](doubleSpendTx)
    testProbe.expectMsgClass(cleanupDuration, newTx)
    getPoolSize shouldBe 2

    val _: ActorRef = MempoolAuditorRef(nodeViewHolderRef, nodeViewHolderRef, settingsToTest)

    // include first transaction in the block so that second tx becomes invalid
    val block = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    applyBlock(block) shouldBe 'success

    getPoolSize shouldBe 1 // first tx removed from pool during node view update

    scorex.core.utils.untilTimeout(cleanupDuration * 2, 100.millis) {
      getPoolSize shouldBe 0 // another tx invalidated by `MempoolAuditor`
    }

  }

}
