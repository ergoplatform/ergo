package org.ergoplatform.network

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.InputBlockTransactionIdsTypeId
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.Message
import org.ergoplatform.network.message.inputblocks.{InputBlockMessageSpec, InputBlockTransactionIdsData, InputBlockTransactionIdsMessageSpec, InputBlockTransactionsData, InputBlockTransactionsRequest}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer}
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.AkkaFixture

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class MatrixInputIdAdmissionSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._

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

  property("stored IDs require complete bodies matching the proved announcement digest") {
    withFixture { f =>
      val first = validErgoTransactionGenTemplate(0, 0).sample.get._2
      val second = validErgoTransactionGenTemplate(0, 0).sample.get._2
      (first.id != second.id) shouldBe true
      val expected = Seq(first.id, second.id)
      val digest = Algos.merkleTreeRoot(Seq(first, second).map(tx => LeafData @@ tx.serializedId))
      val previousDigest = Digest32 @@ Array.fill(32)(0.toByte)
      val proof = InputBlockFields.toExtensionFields(None, digest, previousDigest)
        .proofForInputBlockData.get
      proof.indices.nonEmpty shouldBe true
      val fields = new InputBlockFields(None, digest, previousDigest, proof)
      val announcement = f.announcement.copy(inputBlockFields = fields)
      val complete = ErgoNodeViewSynchronizer.inputBlockTransactionsComplete _

      complete(None, Some(expected), Some(Seq(first, second))) shouldBe false
      complete(Some(announcement), None, Some(Seq.empty)) shouldBe false
      complete(Some(announcement), Some(Seq.empty), Some(Seq.empty)) shouldBe false
      complete(Some(announcement), Some(expected), Some(Seq(first))) shouldBe false
      complete(Some(announcement), Some(expected), Some(Seq(second, first))) shouldBe false
      complete(Some(announcement), Some(expected), Some(Seq(first, second))) shouldBe true
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
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id)
        .map(_.peer.handlerRef) shouldBe Some(f.peer.handlerRef)
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
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id)
        .map(_.peer.handlerRef) shouldBe Some(f.peer.handlerRef)
      f.process()
      f.requests should have size 1
    }
  }

  property("known incomplete announcements can resume transaction resolution") {
    withFixture { f =>
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}

      f.history.applyInputBlock(f.announcement)
      f.history.getInputBlock(f.announcement.id).isDefined shouldBe true
      // Stored history alone does not authorize a peer to supply an ID list.
      f.process()
      f.requests shouldBe empty

      val state = boxesHolderGen
        .map(WrappedUtxoState(_, createTempDir, parameters, f.cfg)).sample.get
      f.ref ! ChangedState(state)
      f.ref ! ChangedHistory(f.history)
      f.ref ! ChangedMempool(f.pool)
      f.ref ! Message(InputBlockMessageSpec,
        Left(InputBlockMessageSpec.toBytes(f.announcement)), Some(f.peer))
      val request = f.nc.fishForMessage(3.seconds) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == InputBlockTransactionIdsTypeId.value && inv.ids == Seq(f.announcement.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      request.sendingStrategy shouldBe SendToPeer(f.peer)
      f.ref ! Message(InputBlockTransactionIdsMessageSpec,
        Left(InputBlockTransactionIdsMessageSpec.toBytes(f.ids)), Some(f.peer))
      f.requests should have size 1
    }
  }

  property("an unsolicited later ID list cannot replace cached weak IDs") {
    withFixture { f =>
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ProcessInputBlockTransactions

      val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
      f.history.applyInputBlock(f.announcement)
      f.sync.requestInputBlockTransactionIds(f.announcement, f.peer)
      val acceptedIds = InputBlockTransactionIdsData(f.announcement.id, Seq(tx.weakId))
      f.sync.processInputBlockTransactionIds(acceptedIds, f.history, f.pool, f.peer)
      f.requests should have size 1
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id) shouldBe None

      val conflictingId = tx.weakId.clone()
      conflictingId(0) = (conflictingId(0) ^ 1).toByte
      val unsolicitedIds = InputBlockTransactionIdsData(f.announcement.id, Seq(conflictingId))
      f.sync.processInputBlockTransactionIds(unsolicitedIds, f.history, f.pool, f.peer)

      f.sync.processInputBlockTransactions(
        InputBlockTransactionsData(f.announcement.id, Seq(tx)), f.history, f.peer)
      val completed = f.vh.fishForMessage(1.second) {
        case _: ProcessInputBlockTransactions => true
        case _ => false
      }.asInstanceOf[ProcessInputBlockTransactions]
      completed.std.transactions.length shouldBe 1
      completed.std.transactions.head.weakId.sameElements(tx.weakId) shouldBe true
      f.requests shouldBe empty
    }
  }

  property("ID request retry carries its delivery-check budget") {
    withFixture { f =>
      f.sync.requestInputBlockTransactionIds(f.announcement, f.peer, checksDone = 2)
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id).map(_.checks) shouldBe Some(2)
    }
  }

  property("ID timeout retries while the announcement is still awaiting persistence") {
    withFixture { f =>
      import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, CheckDelivery}
      import org.ergoplatform.network.message.{InvData, RequestModifierSpec}

      f.ref ! ChangedHistory(f.history)
      f.ref ! ChangedMempool(f.pool)
      f.sync.requestInputBlockTransactionIds(f.announcement, f.peer, checksDone = 1)
      f.nc.fishForMessage(1.second) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          stn.message.data.get.asInstanceOf[InvData].ids == Seq(f.announcement.id)
        case _ => false
      }
      f.history.getInputBlock(f.announcement.id) shouldBe None

      f.ref ! CheckDelivery(f.peer, InputBlockTransactionIdsTypeId.value, f.announcement.id)
      val retry = f.nc.fishForMessage(1.second) {
        case stn: SendToNetwork if stn.message.spec.messageCode == RequestModifierSpec.messageCode =>
          val inv = stn.message.data.get.asInstanceOf[InvData]
          inv.typeId == InputBlockTransactionIdsTypeId.value && inv.ids == Seq(f.announcement.id)
        case _ => false
      }.asInstanceOf[SendToNetwork]
      retry.sendingStrategy shouldBe SendToPeer(f.peer)
      f.tracker.getRequestedInfo(InputBlockTransactionIdsTypeId.value, f.announcement.id)
        .map(_.peer.handlerRef) shouldBe Some(f.peer.handlerRef)
    }
  }
}
