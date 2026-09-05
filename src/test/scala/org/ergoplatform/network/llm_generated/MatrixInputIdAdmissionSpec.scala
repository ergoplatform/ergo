package org.ergoplatform.network.llm_generated

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker}
import org.ergoplatform.network.message.Message
import org.ergoplatform.network.message.inputblocks.{InputBlockTransactionIdsData, InputBlockTransactionsRequest}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer}
import scorex.testkit.utils.AkkaFixture

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class MatrixInputIdAdmissionSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._

  private class Synchronizer(nc: ActorRef, vh: ActorRef, cfg: ErgoSettings,
                             tracker: DeliveryTracker)(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(nc, vh, ErgoSyncInfoMessageSpec, cfg,
      ErgoSyncTracker(cfg.scorexSettings.network), tracker)

  private class Fixture extends AkkaFixture {
    implicit val ec: ExecutionContext = system.dispatcher
    val nc: TestProbe = TestProbe()
    val vh: TestProbe = TestProbe()
    val handler: TestProbe = TestProbe()
    val cfg: ErgoSettings = settings.copy(directory = createTempDir.getAbsolutePath)
    val history: ErgoHistory = ErgoHistory.readOrGenerate(cfg)(null)
    val pool: ErgoMemPool = ErgoMemPool.empty(cfg)
    val tracker: DeliveryTracker = DeliveryTracker.empty(cfg)
    val ref: TestActorRef[Synchronizer] = TestActorRef(Props(new Synchronizer(nc.ref, vh.ref, cfg, tracker)))
    val sync: Synchronizer = ref.underlyingActor
    val peer: ConnectedPeer = ConnectedPeer(connectionIdGen.sample.get, handler.ref,
      Some(PeerInfo(defaultPeerSpec, System.currentTimeMillis())))
    val announcement: InputBlockAnnouncement = InputBlockAnnouncement(1.toByte,
      genChain(1, history).head.header, InputBlockFields.empty, None)
    val ids: InputBlockTransactionIdsData = InputBlockTransactionIdsData(announcement.id,
      Seq(Array.fill(ErgoTransaction.WeakIdLength)(1.toByte)))

    def requests: Seq[SendToNetwork] = nc.receiveWhile(500.millis, 50.millis) {
      case m => m
    }.collect {
      case m @ SendToNetwork(Message(_, Right(_: InputBlockTransactionsRequest), _), _) => m
    }
    def process(): Unit = sync.processInputBlockTransactionIds(ids, history, pool, peer)
  }

  private def withFixture(test: Fixture => Unit): Unit = {
    val fixture = new Fixture
    try test(fixture)
    finally {
      Await.result(fixture.system.terminate(), 10.seconds)
    }
  }

  property("unknown unrequested ID responses produce no body request or view-holder work") {
    withFixture { f =>
      f.process()
      f.requests shouldBe empty
      f.vh.receiveWhile(100.millis, 20.millis) { case m => m }
        .exists(_.isInstanceOf[org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessInputBlockTransactions]) shouldBe false
    }
  }

  property("registered requests admit the expected peer before history persistence") {
    withFixture { f =>
      f.sync.requestInputBlockTransactionIds(f.announcement, f.peer)
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id).map(_.peer) shouldBe Some(f.peer)
      f.process()
      f.requests.map(_.sendingStrategy) shouldBe Seq(SendToPeer(f.peer))
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id) shouldBe None
    }
  }

  property("a different peer cannot consume a pending ID response") {
    withFixture { f =>
      f.sync.requestInputBlockTransactionIds(f.announcement, f.peer)
      val other = f.peer.copy(handlerRef = TestProbe()(f.system).ref)
      f.sync.processInputBlockTransactionIds(f.ids, f.history, f.pool, other)
      f.requests shouldBe empty
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id).map(_.peer) shouldBe Some(f.peer)
      f.process()
      f.requests should have size 1
    }
  }

  property("known incomplete announcements can resume transaction resolution") {
    withFixture { f =>
      f.history.applyInputBlock(f.announcement)
      f.history.getInputBlock(f.announcement.id).isDefined shouldBe true
      f.process()
      f.requests should have size 1
    }
  }

  property("ID request retry carries its delivery-check budget") {
    withFixture { f =>
      f.sync.requestInputBlockTransactionIds(f.announcement, f.peer, checksDone = 2)
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id).map(_.checks) shouldBe Some(2)
    }
  }
}
