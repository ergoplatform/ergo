package org.ergoplatform.local

import akka.actor.ActorRef
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.NodeViewTestOps
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.scalatest.FlatSpec
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.FailedTransaction

import scala.concurrent.duration._

class MempoolAuditorSpec extends FlatSpec with NodeViewTestOps {

  val cleanupDuration: FiniteDuration = 5.seconds
  val settingsToTest: ErgoSettings = settings.copy(
    nodeSettings = settings.nodeSettings.copy(mempoolCleanupDuration = cleanupDuration))
  val fixture = new NodeViewFixture(settingsToTest)

  it should "expose transactions which become invalid" in {
    import fixture._

    val _: ActorRef = MempoolAuditorRef(nodeViewHolderRef, settingsToTest.nodeSettings)

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))
    val genesis = validFullBlock(parentOpt = None, us, bh)
    val wusAfterGenesis = WrappedUtxoState(us, bh, stateConstants).applyModifier(genesis).get

    applyBlock(genesis) shouldBe 'success
    getRootHash shouldBe Algos.encode(wusAfterGenesis.rootHash)

    val boxes = ErgoState.boxChanges(genesis.transactions)._2.find(_.ergoTree == Constants.TrueLeaf)
    boxes.nonEmpty shouldBe true

    val validTx = validTransactionFromBoxes(boxes.toIndexedSeq)
    val doubleSpendTx = validTransactionFromBoxes(boxes.toIndexedSeq)

    subscribeEvents(classOf[FailedTransaction])
    Seq(validTx, doubleSpendTx).foreach(tx => nodeViewHolderRef ! LocallyGeneratedTransaction[ErgoTransaction](tx))
    Thread.sleep(1000)
    expectNoMsg()
    getPoolSize shouldBe 2

    // include first transaction in the block so that second tx becomes invalid
    val block = validFullBlock(Some(genesis), wusAfterGenesis, Seq(validTx))

    applyBlock(block) shouldBe 'success

    getPoolSize shouldBe 1 // first tx removed from pool during node view update

    Thread.sleep(cleanupDuration.toMillis * 2)

    getPoolSize shouldBe 0 // another tx invalidated by `MempoolAuditor`
  }

}
