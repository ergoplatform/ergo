package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import akka.testkit.TestProbe
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.{ErgoMemPoolReader, OrderedTxPool}
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.{ErgoTestHelpers, NodeViewTestOps}
import org.scalatest.FlatSpec
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState, FailedTransaction, SuccessfulTransaction}
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.util.{Random, Success, Try}

class MempoolAuditorSpec extends FlatSpec with NodeViewTestOps with ErgoTestHelpers {

  val cleanupDuration: FiniteDuration = 2.seconds
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

  it should "rebroadcast transactions correctly" in {
    import fixture._

    //networking layer stub which is not doing anything
    class NetworkControllerStub extends Actor {
      def receive: Receive = {
        case _ =>
      }
    }

    val testProbe = new TestProbe(actorSystem)
    actorSystem.eventStream.subscribe(testProbe.ref, newTx)

    val (us, bh) = createUtxoState(Some(nodeViewHolderRef))

    val genesis = validFullBlock(parentOpt = None, us, bh)

    // mempool reader stub specifically for this test
    // only take is defined as only this method is used in rebroadcasting
    object fakeMempool extends ErgoMemPoolReader {
      override type NVCT = this.type

      override def modifierById(modifierId: ModifierId): Option[ErgoTransaction] = ???

      override def getAll(ids: Seq[ModifierId]): Seq[ErgoTransaction] = ???

      override def size: Int = ???

      override def weightedTransactionIds(limit: Int): Seq[OrderedTxPool.WeightedTxId] = ???

      override def getAll: Seq[ErgoTransaction] = ???

      override def getAllPrioritized: Seq[ErgoTransaction] = genesis.transactions

      override def take(limit: Int): Iterable[ErgoTransaction] = genesis.transactions.take(limit)
    }

    implicit val defaultSender: ActorRef = testProbe.testActor

    val auditor: ActorRef = MempoolAuditorRef(nodeViewHolderRef, defaultSender, settingsToTest)

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

    expectMsgType[SendToNetwork]
  }
}
