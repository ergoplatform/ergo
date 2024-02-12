package org.ergoplatform.nodeView

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.ergoplatform.consensus.SyncInfo
import org.ergoplatform.core.BytesSerializable
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{GetNodeViewChanges, ModifiersFromRemote}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState.ManifestId
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.utils.TestFileUtils
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.ConnectedPeer
import scorex.core.network.NetworkController.ReceivableMessages.{PenalizePeer, SendToNetwork}
import org.ergoplatform.network.message._
import org.ergoplatform.network.peer.PenaltyType
import org.ergoplatform.serialization.{ErgoSerializer, ManifestSerializer}
import org.ergoplatform.testkit.generators.{SyntacticallyTargetedModifierProducer, TotallyValidModifierProducer}
import org.ergoplatform.testkit.utils.AkkaFixture
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import scorex.util.serialization.{Reader, Writer}
import org.ergoplatform.utils.generators.ChainGenerator

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

@SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
trait NodeViewSynchronizerTests[ST <: ErgoState[ST]] extends AnyPropSpec
  with Matchers
  with ScorexLogging
  with SyntacticallyTargetedModifierProducer
  with TotallyValidModifierProducer[ST]
  with ChainGenerator
  with TestFileUtils {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  val historyGen: Gen[ErgoHistory]
  val memPool: ErgoMemPool

  val stateGen: Gen[ST]

  def nodeViewSynchronizer(implicit system: ActorSystem):
  (ActorRef, ErgoSyncInfo, BlockSection, ErgoTransaction, ConnectedPeer, TestProbe, TestProbe, TestProbe, TestProbe, ErgoSerializer[BlockSection])

  class SynchronizerFixture extends AkkaFixture {
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val (node, syncInfo, mod, tx, peer, pchProbe, ncProbe, vhProbe, eventListener, modSerializer) = nodeViewSynchronizer
  }

  // ToDo: factor this out of here and NVHTests?
  private def withFixture(testCode: SynchronizerFixture => Any): Unit = {
    val fixture = new SynchronizerFixture
    try {
      testCode(fixture)
    }
    finally {
      Await.result(fixture.system.terminate(), Duration.Inf)
    }
  }

  property("NodeViewSynchronizer: SuccessfulTransaction") {
    withFixture { ctx =>
      import ctx._
      node ! SuccessfulTransaction(UnconfirmedTransaction(tx, None))
      ncProbe.fishForMessage(3 seconds) { case m => m.isInstanceOf[SendToNetwork] }
    }
  }

  property("NodeViewSynchronizer: FailedTransaction") {
    withFixture { ctx =>
      import ctx._
      node ! FailedTransaction(UnconfirmedTransaction(tx, None), new Exception)
      // todo: NVS currently does nothing in this case. Should check banning.
    }
  }

  property("NodeViewSynchronizer: SyntacticallySuccessfulModifier") {
    withFixture { ctx =>
      import ctx._
      node ! SyntacticallySuccessfulModifier(mod.modifierTypeId, mod.id)
      // todo ? : NVS currently does nothing in this case. Should it do?
    }
  }

  property("NodeViewSynchronizer: SyntacticallyFailedModification") {
    withFixture { ctx =>
      import ctx._
      node ! SyntacticallyFailedModification(mod.modifierTypeId, mod.id, new Exception)
      // todo: NVS currently does nothing in this case. Should check banning.
    }
  }

  property("NodeViewSynchronizer: SemanticallySuccessfulModifier") {
    withFixture { ctx =>
      import ctx._
      node ! FullBlockApplied(mod.asInstanceOf[Header]) //todo: fix
      ncProbe.fishForMessage(3 seconds) { case m => m.isInstanceOf[SendToNetwork] }
    }
  }

  property("NodeViewSynchronizer: SemanticallyFailedModification") {
    withFixture { ctx =>
      import ctx._
      node ! SemanticallyFailedModification(mod.modifierTypeId, mod.id, new Exception)
      // todo: NVS currently does nothing in this case. Should check banning.
    }
  }

  //TODO rewrite
  ignore("NodeViewSynchronizer: Message: SyncInfoSpec") {
    withFixture { ctx =>
      import ctx._

      val dummySyncInfoMessageSpec = new SyncInfoMessageSpec[SyncInfo](serializer = new ErgoSerializer[SyncInfo] {
        override def parse(r: Reader): SyncInfo = {
          throw new Exception()
        }

        override def serialize(obj: SyncInfo, w: Writer): Unit = {}
      })

      val dummySyncInfo: SyncInfo = new SyncInfo {
        type M = BytesSerializable

        def serializer: ErgoSerializer[M] = throw new Exception
      }

      val msgBytes = dummySyncInfoMessageSpec.toBytes(dummySyncInfo)

      node ! Message(dummySyncInfoMessageSpec, Left(msgBytes), Some(peer))
      //    vhProbe.fishForMessage(3 seconds) { case m => m == OtherNodeSyncingInfo(peer, dummySyncInfo) }
    }
  }

  property("NodeViewSynchronizer: GetNipopowProof") {
    withFixture { ctx =>
      import ctx._

      // Generate history chain
      val emptyHistory = historyGen.sample.get
      val prefix = blockStream(None).take(settings.chainSettings.makeSnapshotEvery)
      val fullHistory = applyChain(emptyHistory, prefix)

      // Broadcast updated history
      node ! ChangedHistory(fullHistory)

      // Build and send GetNipopowProofSpec request
      val spec = GetNipopowProofSpec
      val msgBytes = spec.toBytes(NipopowProofData(m = emptyHistory.P2PNipopowProofM, k = emptyHistory.P2PNipopowProofK, headerId = None))
      node ! Message[NipopowProofData](spec, Left(msgBytes), Option(peer))

      // Listen for NipopowProofSpec response
      ncProbe.fishForMessage(5 seconds) {
        case stn: SendToNetwork =>
          stn.message.spec match {
            case _: NipopowProofSpec.type => true
            case _ => false
          }
        case _: Any => false
      }
    }
  }

  property("NodeViewSynchronizer: Message: InvSpec") {
    withFixture { ctx =>
      import ctx._
      val syncMsgBytes = ErgoSyncInfoMessageSpec.toBytes(syncInfo)
      node ! Message(ErgoSyncInfoMessageSpec, Left(syncMsgBytes), Some(peer))

      val spec = InvSpec
      val modifiers = Seq(mod.id)
      val msgBytes = spec.toBytes(InvData(mod.modifierTypeId, modifiers))
      node ! Message(spec, Left(msgBytes), Some(peer))
      ncProbe.fishForMessage(5 seconds) {
        case SendToNetwork(msg, _)
          if msg.spec.messageCode == RequestModifierSpec.messageCode &&
            msg.data.get.asInstanceOf[InvData].ids.head == mod.id => true
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: Message: RequestModifierSpec") {
    withFixture { ctx =>
      import ctx._
      @SuppressWarnings(Array("org.wartremover.warts.OptionPartial"))
      val h = historyGen.sample.get
      val mod = syntacticallyValidModifier(h)
      val (newH, _) = h.append(mod).get
      val m = memPool
      val spec = RequestModifierSpec
      val modifiers = Seq(mod.id)
      val msgBytes = spec.toBytes(InvData(mod.modifierTypeId, modifiers))
      node ! ChangedHistory(newH)
      node ! ChangedMempool(m)
      node ! Message(spec, Left(msgBytes), Option(peer))

      pchProbe.fishForMessage(5 seconds) {
        case _: Message[_] => true
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: Message: Non-Asked Modifiers from Remote") {
    withFixture { ctx =>
      import ctx._

      val modifiersSpec = ModifiersSpec
      val msgBytes = modifiersSpec.toBytes(ModifiersData(mod.modifierTypeId, Map(mod.id -> mod.bytes)))

      node ! Message(modifiersSpec, Left(msgBytes), Option(peer))
      val messages = vhProbe.receiveWhile(max = 3 seconds, idle = 1 second) { case m => m }
      assert(!messages.exists(_.isInstanceOf[ModifiersFromRemote]))
    }
  }

  property("NodeViewSynchronizer: Message: Asked Modifiers from Remote") {
    withFixture { ctx =>
      import ctx._
      vhProbe.expectMsgType[GetNodeViewChanges]

      val invSpec = InvSpec
      val invMsgBytes = invSpec.toBytes(InvData(mod.modifierTypeId, Seq(mod.id)))

      val modifiersSpec = ModifiersSpec
      val modMsgBytes = modifiersSpec.toBytes(ModifiersData(mod.modifierTypeId, Map(mod.id -> mod.bytes)))

      node ! Message(invSpec, Left(invMsgBytes), Option(peer))
      node ! Message(modifiersSpec, Left(modMsgBytes), Option(peer))
      vhProbe.fishForMessage(3 seconds) {
        case m: ModifiersFromRemote => m.modifiers.toSeq.contains(mod)
        case _ => false
      }
    }
  }

  property("NodeViewSynchronizer: Message - CheckDelivery -  Do not penalize if delivered") {
    withFixture { ctx =>
      import ctx._

      val invSpec = InvSpec
      val invMsgBytes = invSpec.toBytes(InvData(mod.modifierTypeId, Seq(mod.id)))

      val modifiersSpec = ModifiersSpec
      val modMsgBytes = modifiersSpec.toBytes(ModifiersData(mod.modifierTypeId, Map(mod.id -> mod.bytes)))

      node ! Message(invSpec, Left(invMsgBytes), Option(peer))
      node ! Message(modifiersSpec, Left(modMsgBytes), Option(peer))
      system.scheduler.scheduleOnce(1 second, node, Message(modifiersSpec, Left(modMsgBytes), Option(peer)))
      val messages = ncProbe.receiveWhile(max = 5 seconds, idle = 1 second) { case m => m }
      assert(!messages.contains(PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)))
    }
  }


  property("NodeViewSynchronizer: GetSnapshotInfo") {
    withFixture { ctx =>
      import ctx._

      val s = stateGen.sample.get

      if (s.isInstanceOf[UtxoStateReader]) {
        // To initialize utxoStateReaderOpt in ErgoNodeView Synchronizer
        node ! ChangedState(s)

        // First, store snapshots info in DB
        val m = (0 until 100).map { _ =>
          Random.nextInt(1000000) -> (Digest32 @@ Algos.decode(mod.id).get)
        }.toMap
        val si = new SnapshotsInfo(m)
        val db = SnapshotsDb.create(createTempDir.getPath)
        db.writeSnapshotsInfo(si)

        // Then send message to request it
        node ! Message[Unit](GetSnapshotsInfoSpec, Left(Array.empty[Byte]), Option(peer))
        ncProbe.fishForMessage(5 seconds) {
          case stn: SendToNetwork if stn.message.spec.isInstanceOf[SnapshotsInfoSpec.type] => true
          case _: Any => false
        }
      } else {
        log.info("Snapshots not supported by digest-state")
      }
    }
  }

  property("NodeViewSynchronizer: GetManifest") {
    withFixture { ctx =>
      import ctx._

      val s = stateGen.sample.get

      s match {
        case usr: UtxoState => {
          // To initialize utxoStateReaderOpt in ErgoNodeView Synchronizer
          node ! ChangedState(s)

          // Generate some snapshot
          val height = 1
          usr.applyModifier(mod, Some(height))(_ => ())

          val manifestId = usr.dumpSnapshot(height, usr.rootDigest.dropRight(1)).get

          // Then send message to request it
          node ! Message[ManifestId](GetManifestSpec, Left(manifestId), Option(peer))
          ncProbe.fishForMessage(5 seconds) {
            case stn: SendToNetwork if stn.message.spec.isInstanceOf[ManifestSpec.type] => true
            case _: Any => false
          }
        }
        case _ =>
          log.info("Snapshots not supported by digest-state")
      }
    }
  }

  property("NodeViewSynchronizer: GetSnapshotChunk") {
    withFixture { ctx =>
      import ctx._

      val s = stateGen.sample.get

      s match {
        case usr: UtxoState => {
          // To initialize utxoStateReaderOpt in ErgoNodeView Synchronizer
          node ! ChangedState(s)

          // Generate some snapshot

          val height = 1

          usr.applyModifier(mod, Some(height))(_ => ())

          val manifestDepth = 2.toByte
          val serializer = new ManifestSerializer(manifestDepth)
          usr.dumpSnapshot(height, usr.rootDigest.dropRight(1), manifestDepth)
          val manifestId = usr.snapshotsDb.readSnapshotsInfo.availableManifests.apply(height)
          val manifestBytes = usr.snapshotsDb.readManifestBytes(manifestId).get
          val manifest = serializer.parseBytes(manifestBytes)
          val subtreeIds = manifest.subtreesIds

          // Then send message to request it
          node ! Message[ManifestId](GetUtxoSnapshotChunkSpec, Left(subtreeIds.last), Option(peer))
          ncProbe.fishForMessage(5 seconds) {
            case stn: SendToNetwork if stn.message.spec.isInstanceOf[UtxoSnapshotChunkSpec.type] => true
            case _: Any => false
          }
        }
        case _ =>
          log.info("Snapshots not supported by digest-state")
      }
    }
  }

}
