package org.ergoplatform.network

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
  ChangedHistory,
  ChangedMempool,
  ProcessInputBlockTransactions
}
import org.ergoplatform.network.message.Message
import org.ergoplatform.network.message.inputblocks.{
  InputBlockTransactionIdsData,
  InputBlockTransactionIdsMessageSpec,
  InputBlockTransactionsData,
  InputBlockTransactionsRequest,
  InputBlockTransactionsRequestMessageSpec
}
import org.ergoplatform.network.peer.{PeerInfo, PenaltyType}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.{
  PenalizePeer,
  SendToNetwork
}
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.testkit.utils.AkkaFixture
import scorex.util.{bytesToId, ModifierId}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

/**
  * Weak ids in a list come from a peer. The cost of matching them must grow linearly with the list.
  *
  * The time limits are far above the linear cost (under 1 s on a laptop). They are below the cost of
  * pairwise matching on the same laptop: 128 s for the first test, 14 s for the second, 47 s for the third.
  */
class WeakIdMatchingSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

  class SynchronizerUnderTest(
    networkControllerRef: ActorRef,
    viewHolderRef: ActorRef,
    settings: ErgoSettings,
    syncTracker: ErgoSyncTracker,
    deliveryTracker: DeliveryTracker
  )(implicit ec: ExecutionContextExecutor)
    extends ErgoNodeViewSynchronizer(
      networkControllerRef,
      viewHolderRef,
      ErgoSyncInfoMessageSpec,
      settings,
      syncTracker,
      deliveryTracker
    )(ec)

  class Fixture(mempoolTxs: Seq[ErgoTransaction]) extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    private val localSettings: ErgoSettings =
      settings.copy(directory = createTempDir.getAbsolutePath)

    val ncProbe         = TestProbe("NetworkControllerProbe")
    val viewHolderProbe = TestProbe("ViewHolderProbe")

    val synchronizer: TestActorRef[SynchronizerUnderTest] = TestActorRef(
      Props(
        new SynchronizerUnderTest(
          ncProbe.ref,
          viewHolderProbe.ref,
          localSettings,
          ErgoSyncTracker(localSettings.scorexSettings.network),
          DeliveryTracker.empty(localSettings)
        )
      )
    )

    val peer: ConnectedPeer = ConnectedPeer(
      connectionIdGen.sample.get,
      TestProbe("PeerHandlerProbe").ref,
      Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis()))
    )

    val hist: ErgoHistory = ErgoHistory.readOrGenerate(localSettings)(null)

    val mempool: ErgoMemPool = mempoolTxs.foldLeft(ErgoMemPool.empty(localSettings)) {
      (mp, tx) =>
        mp.put(UnconfirmedTransaction(tx, None))
    }

    synchronizer ! ChangedHistory(hist)
    synchronizer ! ChangedMempool(mempool)

    def processIds(blockId: ModifierId, ids: Seq[Array[Byte]]): Unit =
      synchronizer.underlyingActor.processInputBlockTransactionIds(
        InputBlockTransactionIdsData(blockId, ids),
        mempool,
        peer
      )

    def outgoing(): Seq[Any] = ncProbe.receiveWhile(max = 3.seconds, idle = 300.millis) {
      case m => m
    }
  }

  private def withFixture(
    mempoolTxs: Seq[ErgoTransaction]
  )(test: Fixture => Any): Unit = {
    val f = new Fixture(mempoolTxs)
    try test(f)
    finally Await.result(f.system.terminate(), Duration.Inf)
  }

  private val rnd = new java.util.Random(2899L)

  private def randomIds(n: Int): Vector[Array[Byte]] = Vector.fill(n) {
    val id = new Array[Byte](ErgoTransaction.WeakIdLength)
    rnd.nextBytes(id)
    id
  }

  private def randomBlockId(): ModifierId = {
    val id = new Array[Byte](32)
    rnd.nextBytes(id)
    bytesToId(id)
  }

  private def tx(): ErgoTransaction =
    Iterator.continually(invalidErgoTransactionGen.sample).flatten.next()

  private def seconds(block: => Unit): Double = {
    val start = System.nanoTime()
    block
    (System.nanoTime() - start) / 1e9
  }

  private def requestedIds(out: Seq[Any]): Seq[Int] = out.collect {
    case SendToNetwork(m, _)
        if m.spec.messageCode == InputBlockTransactionsRequestMessageSpec.messageCode =>
      m.data.get.asInstanceOf[InputBlockTransactionsRequest].txIds.length
  }

  private def penalties(out: Seq[Any]): Seq[PenalizePeer] = out.collect {
    case p: PenalizePeer => p
  }

  property("a list with repeated ids is matched in linear time, with no penalty") {
    val mempoolTx = tx()
    withFixture(Seq(mempoolTx)) { f =>
      val k   = 100000
      val ids = Vector.fill(k)(mempoolTx.weakId) ++ randomIds(k)
      seconds(f.processIds(randomBlockId(), ids)) should be < 10.0

      val out = f.outgoing()
      requestedIds(out) shouldBe Seq(k)
      penalties(out) shouldBe empty
    }
  }

  property("a missing id does not scan the whole mempool") {
    withFixture(Vector.fill(1000)(tx())) { f =>
      val n = 2731428
      seconds(f.processIds(randomBlockId(), randomIds(n))) should be < 5.0

      requestedIds(f.outgoing()) shouldBe Seq(n)
    }
  }

  property("a body is merged with a long list in linear time") {
    val mempoolTx = tx()
    withFixture(Seq(mempoolTx)) { f =>
      val k       = 100000
      val blockId = randomBlockId()
      f.processIds(blockId, Vector.fill(k)(mempoolTx.weakId) ++ randomIds(k))

      val body = InputBlockTransactionsData(blockId, Seq(tx()))
      seconds(
        f.synchronizer.underlyingActor.processInputBlockTransactions(body, f.hist, f.peer)
      ) should be < 10.0

      val toViewHolder =
        f.viewHolderProbe.receiveWhile(max = 1.second, idle = 300.millis) { case m => m }
      toViewHolder.collect { case p: ProcessInputBlockTransactions => p } shouldBe empty
    }
  }

  property(
    "repeated ids of a mempool transaction are resolved as before, with no penalty"
  ) {
    val mempoolTx = tx()
    withFixture(Seq(mempoolTx)) { f =>
      val blockId = randomBlockId()
      f.processIds(blockId, Seq.fill(3)(mempoolTx.weakId))

      val delivered = f.viewHolderProbe
        .fishForMessage(5.seconds) {
          case _: ProcessInputBlockTransactions => true
          case _                                => false
        }
        .asInstanceOf[ProcessInputBlockTransactions]
      delivered.std.inputBlockId shouldBe blockId
      delivered.std.transactions.map(_.id) shouldBe Seq.fill(3)(mempoolTx.id)
      penalties(f.outgoing()) shouldBe empty
    }
  }

  property(
    "a list above the limit gets a permanent penalty, a list at the limit does not"
  ) {
    withFixture(Seq.empty) { f =>
      val max = InputBlockTransactionIdsMessageSpec.MaxTransactionIds

      def send(ids: Seq[Array[Byte]]): Unit = {
        val bytes = InputBlockTransactionIdsMessageSpec.toBytes(
          InputBlockTransactionIdsData(randomBlockId(), ids)
        )
        f.synchronizer ! Message(
          InputBlockTransactionIdsMessageSpec,
          Left(bytes),
          Some(f.peer)
        )
      }

      send(randomIds(max))
      val atLimit = f.outgoing()
      requestedIds(atLimit) shouldBe Seq(max)
      penalties(atLimit) shouldBe empty

      send(randomIds(max + 1))
      val aboveLimit = f.outgoing()
      requestedIds(aboveLimit) shouldBe empty
      penalties(aboveLimit).map(_.penaltyType) shouldBe Seq(PenaltyType.PermanentPenalty)
    }
  }
}
