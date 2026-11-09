package org.ergoplatform.network

import akka.actor.{ActorRef, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.consensus.Older
import org.ergoplatform.modifiers.SnapshotsInfoTypeId
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.message.{GetManifestSpec, GetSnapshotsInfoSpec, Message, SnapshotsInfoSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{SnapshotsInfo, StateType, UtxoState}
import org.ergoplatform.settings.{Algos, ErgoSettings, UtxoSettings}
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage.splitDigest
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.AkkaFixture
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutor}

class SnapshotsInfoAdmissionSpec extends AnyPropSpec with Matchers with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators._

  private class InjectedReadersNodeViewHolder(history: ErgoHistory, mempool: ErgoMemPool)
    extends ErgoNodeViewHolder[UtxoState](settings) {
    override protected def getNodeViewChanges: Receive = {
      case request: ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges =>
        if (request.history) sender() ! ChangedHistory(history)
        super.getNodeViewChanges(request.copy(history = false, mempool = false))
        if (request.mempool) sender() ! ChangedMempool(mempool)
    }
  }

  private class Fixture(bootstrap: Boolean) extends AkkaFixture {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    val networkProbe = TestProbe("NetworkControllerProbe")
    val peerHandlerProbe = TestProbe("PeerHandlerProbe")
    val synchronizerSettings: ErgoSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(utxoSettings = UtxoSettings(bootstrap, 0, 1))
    )
    val syncTracker = ErgoSyncTracker(synchronizerSettings.scorexSettings.network)
    val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(synchronizerSettings)

    deleteRecursive(ErgoHistory.historyDir(synchronizerSettings))
    val emptyHistory = generateHistory(verifyTransactions = true, StateType.Utxo,
      PoPoWBootstrap = false, blocksToKeep = -1, utxoBootstrap = bootstrap)
    val history = applyHeaderChain(emptyHistory,
      genHeaderChain(10, emptyHistory, diffBitsOpt = None, useRealTs = false))
    val mempool = ErgoMemPool.empty(synchronizerSettings)
    val holder: ActorRef = system.actorOf(Props(new InjectedReadersNodeViewHolder(history, mempool)))
    val synchronizer: TestActorRef[ErgoNodeViewSynchronizer] = TestActorRef(Props(
      new ErgoNodeViewSynchronizer(networkProbe.ref, holder, ErgoSyncInfoMessageSpec,
        synchronizerSettings, syncTracker, deliveryTracker)
    ))
    synchronizer ! ChangedHistory(history)
    synchronizer ! ChangedMempool(mempool)

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val peer = ConnectedPeer(connectionIdGen.sample.get, peerHandlerProbe.ref,
      Some(PeerInfo(defaultPeerSpec.copy(features = Seq(ModePeerFeature(
        StateType.Utxo, true, None, ModePeerFeature.AllBlocksKept))),
        System.currentTimeMillis())))

    @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
    val manifestId = Digest32 @@ splitDigest(history.bestHeaderAtHeight(1).get.stateRoot)._1

    def sendInfo(): Unit = {
      val bytes = SnapshotsInfoSpec.toBytes(new SnapshotsInfo(Map(1 -> manifestId)))
      synchronizer ! Message(SnapshotsInfoSpec, Left(bytes), Some(peer))
    }

    def requestInfo(): Unit = {
      syncTracker.updateStatus(peer, Older, Some(history.headersHeight))
      val id = ModifierId @@ Algos.encode(manifestId)
      synchronizer ! ErgoNodeViewHolder.DownloadRequest(Map(SnapshotsInfoTypeId.value -> Seq(id)))
      networkProbe.fishForMessage(3.seconds) {
        case send: SendToNetwork => send.message.spec.messageCode == GetSnapshotsInfoSpec.messageCode
        case _ => false
      }
    }

    def counts: (Int, Int) =
      (synchronizer.underlyingActor.pendingSnapshotInfoCount,
        synchronizer.underlyingActor.snapshotOfferCount)
  }

  private def withFixture(bootstrap: Boolean)(test: Fixture => Unit): Unit = {
    val fixture = new Fixture(bootstrap)
    try test(fixture)
    finally Await.result(fixture.system.terminate(), Duration.Inf)
  }

  property("an ordinary node ignores snapshot metadata even if a request marker exists") {
    withFixture(bootstrap = false) { f =>
      f.requestInfo()
      f.counts shouldBe (1, 0)
      f.sendInfo()
      f.counts shouldBe (1, 0)
    }
  }

  property("a bootstrap node ignores snapshot metadata from a peer it did not ask") {
    withFixture(bootstrap = true) { f =>
      f.sendInfo()
      f.counts shouldBe (0, 0)
    }
  }

  property("a bootstrap node accepts one reply from a peer it asked") {
    withFixture(bootstrap = true) { f =>
      f.history.fullBlockHeight shouldBe 0
      f.requestInfo()
      f.counts shouldBe (1, 0)
      f.sendInfo()
      f.counts shouldBe (0, 1)
      f.sendInfo()
      f.counts shouldBe (0, 1)
      val sent = f.networkProbe.receiveWhile(max = 500.millis, idle = 250.millis) { case m => m }
      sent.count {
        case send: SendToNetwork => send.message.spec.messageCode == GetManifestSpec.messageCode
        case _ => false
      } shouldBe 1
    }
  }

  property("disconnect invalidates a pending snapshot metadata reply") {
    withFixture(bootstrap = true) { f =>
      f.requestInfo()
      f.synchronizer ! DisconnectedPeer(f.peer)
      f.counts shouldBe (0, 0)
      f.sendInfo()
      f.counts shouldBe (0, 0)
    }
  }
}
