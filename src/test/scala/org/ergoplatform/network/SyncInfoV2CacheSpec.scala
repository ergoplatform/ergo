package org.ergoplatform.network

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.consensus.Equal
import org.ergoplatform.modifiers.history.header.{Header, HeaderSerializer}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.SyncInfoV2Cache
import org.ergoplatform.network.message.{Message, ModifiersData}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ModifiersFromRemote
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoNodeTestConstants.{defaultPeerSpec, settings}
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.generators.ChainGenerator.{applyHeaderChain, genHeaderChain}
import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectionIdGen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer, SendToPeers}
import scorex.util.ModifierId

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class SyncInfoV2CacheSpec extends AnyPropSpec with Matchers {
  private val headers = genHeaderChain(17, diffBitsOpt = None, useRealTs = false).headers
  private val tip = headers.last
  private val fullSummary = ErgoSyncInfoV2(Seq(tip, headers.head))
  private val reducedSummary = ErgoSyncInfoV2(Seq(tip))

  property("reuse a summary only for the same tip and requested mode") {
    Seq(false, true).foreach { full =>
      val cache = new SyncInfoV2Cache
      val expected = if (full) fullSummary else reducedSummary
      cache.getOrElseUpdate(Some(tip.id), full)(expected) shouldBe expected
      cache.getOrElseUpdate(Some(tip.id), full) {
        fail("An unchanged tip and mode should reuse the cached summary")
      } shouldBe expected
    }
  }

  property("alternating reduced and full requests preserves each requested summary") {
    val cache = new SyncInfoV2Cache
    Seq(false, true, false, true).foreach { full =>
      val expected = if (full) fullSummary else reducedSummary
      cache.getOrElseUpdate(Some(tip.id), full)(expected) shouldBe expected
    }
  }

  property("a different best header at the same height replaces the cached summary") {
    val otherTip = genHeaderChain(1, prefixOpt = Some(headers(headers.size - 2)),
      diffBitsOpt = None, useRealTs = false).last
    otherTip.height shouldBe tip.height
    otherTip.id should not be tip.id

    Seq(false, true).foreach { full =>
      val cache = new SyncInfoV2Cache
      val original = if (full) fullSummary else reducedSummary
      val replacement = ErgoSyncInfoV2(if (full) Seq(otherTip, headers.head) else Seq(otherTip))
      cache.getOrElseUpdate(Some(tip.id), full)(original) shouldBe original
      cache.getOrElseUpdate(Some(otherTip.id), full)(replacement) shouldBe replacement
      cache.getOrElseUpdate(Some(otherTip.id), full) {
        fail("The replacement tip should now be cached")
      } shouldBe replacement
    }
  }

  property("empty history and populated history do not share cached summaries") {
    val cache = new SyncInfoV2Cache
    val empty = ErgoSyncInfoV2(Nil)
    cache.getOrElseUpdate(None, full = true)(empty) shouldBe empty
    cache.getOrElseUpdate(None, full = true) {
      fail("An unchanged empty history should reuse its summary")
    } shouldBe empty
    cache.getOrElseUpdate(Some(tip.id), full = true)(fullSummary) shouldBe fullSummary
    cache.getOrElseUpdate(None, full = true)(empty) shouldBe empty
  }

  private class Synchronizer(nc: ActorRef, vh: ActorRef, cfg: ErgoSettings,
                             tracker: ErgoSyncTracker, delivery: DeliveryTracker)(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(nc, vh, ErgoSyncInfoMessageSpec, cfg,
      tracker, delivery) {
    // Exercise the production entry points without unrelated periodic tasks.
    override def preStart(): Unit = ()

    def receiveHeader(history: ErgoHistory, header: Header, peer: ConnectedPeer): Unit = {
      delivery.setRequested(Header.modifierTypeId, header.id, peer) { check =>
        context.system.scheduler.scheduleOnce(1.minute, self, check)
      }
      val response = ModifiersData(Header.modifierTypeId, Map(header.id -> HeaderSerializer.toBytes(header)))
      modifiersFromRemote(history, ErgoMemPool.empty(cfg), response, peer, FixedSizeApproximateCacheQueue.empty(1))
    }

    def broadcast(history: ErgoHistory): Unit = sendSync(history)

    def reply(history: ErgoHistory, peer: ConnectedPeer): Unit =
      processSyncV2(history, history.syncInfoV2(full = false), peer)
  }

  for {
    fullRequest <- Seq("periodic broadcast", "peer reply")
    startWithFull <- Seq(false, true)
  } {
    property(s"V2 cache preserves $fullRequest after header delivery (full first: $startWithFull)") {
      implicit val system: ActorSystem = ActorSystem("sync-v2-cache")
      implicit val ec: ExecutionContext = system.dispatcher
      val history = generateHistory(verifyTransactions = false, StateType.Utxo,
        PoPoWBootstrap = false, blocksToKeep = -1)
      try {
        // All four ladder entries exist. The delivered header is not applied yet:
        // the actor sends it to the view holder asynchronously, then asks for a reduced summary.
        val chain = genHeaderChain(514, history, diffBitsOpt = None, useRealTs = false)
        applyHeaderChain(history, chain.take(513))
        history.headersHeight shouldBe 513
        val expectedFull = Seq(513, 497, 385, 1).map(h => chain.headers(h - 1).id)
        history.syncInfoV2(full = true).lastHeaders.map(_.id) shouldBe expectedFull
        val nc = TestProbe()
        val vh = TestProbe()
        val peer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref,
          Some(PeerInfo(defaultPeerSpec.copy(protocolVersion = Version(6, 0, 6)), 0L)))
        val tracker = ErgoSyncTracker(settings.scorexSettings.network)
        tracker.updateStatus(peer, Equal, Some(513))
        val ref = TestActorRef[Synchronizer](Props(
          new Synchronizer(nc.ref, vh.ref, settings, tracker, DeliveryTracker.empty(settings))))
        val sync = ref.underlyingActor

        def full(expectedIds: Seq[ModifierId] = expectedFull): Unit = {
          // Make this Equal peer due for periodic sync without a wall-clock sleep.
          tracker.statuses.update(peer,
            tracker.statuses(peer).copy(lastSyncSentTime = Some(System.currentTimeMillis() - 61000L)))
          if (fullRequest == "periodic broadcast") sync.broadcast(history) else sync.reply(history, peer)
          val sent = nc.expectMsgType[SendToNetwork](3.seconds)
          sent.message match {
            case Message(ErgoSyncInfoMessageSpec, Right(info: ErgoSyncInfoV2), _) =>
              info.lastHeaders.map(_.id) shouldBe expectedIds
            case other => fail(s"Expected a full V2 summary, got $other")
          }
          sent.sendingStrategy shouldBe
            (if (fullRequest == "periodic broadcast") SendToPeers(Seq(peer)) else SendToPeer(peer))
        }

        // Cover both reduced -> full and full -> reduced -> full contamination.
        if (startWithFull) full()
        sync.receiveHeader(history, chain.last, peer)
        vh.expectMsgType[ModifiersFromRemote](3.seconds).modifiers.map(_.id).toSeq shouldBe Seq(chain.last.id)
        val reduced = nc.expectMsgType[SendToNetwork](3.seconds)
        reduced.message match {
          case Message(ErgoSyncInfoMessageSpec, Right(info: ErgoSyncInfoV2), _) =>
            info.lastHeaders.map(_.id) shouldBe Seq(chain.headers(512).id)
          case other => fail(s"Expected a reduced V2 summary, got $other")
        }
        history.headersHeight shouldBe 513
        full()

        // Applying the delivered header must invalidate a full summary even though the mode is unchanged.
        history.append(chain.last).get
        history.headersHeight shouldBe 514
        full(Seq(514, 498, 386, 2).map(h => chain.headers(h - 1).id))
      } finally {
        try Await.result(system.terminate(), 10.seconds)
        finally history.closeStorage()
      }
    }
  }
}
