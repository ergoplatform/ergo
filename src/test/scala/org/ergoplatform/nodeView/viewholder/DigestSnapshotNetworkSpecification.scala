package org.ergoplatform.nodeView

import akka.actor.{ActorSystem, Cancellable}
import akka.testkit.TestProbe
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import java.io.File
import java.nio.file.Files
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.UtxoSnapshotChunkTypeId
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, ChangedState, InitStateFromSnapshot}
import org.ergoplatform.network.message.{GetManifestSpec, GetUtxoSnapshotChunkSpec, Message, UtxoSnapshotChunkSpec}
import org.ergoplatform.network.peer.PenaltyType
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, StateType, UtxoState}
import org.ergoplatform.serialization.{ManifestSerializer, SubtreeSerializer}
import org.ergoplatform.settings.{Algos, ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.generators.ChainGenerator.genHeaderChain
import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectedPeerGen
import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.boxesHolderGenOfSize
import org.ergoplatform.wallet.utils.FileUtils
import scorex.core.network.DeliveryTracker
import scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage.splitDigest
import scorex.crypto.hash.Digest32
import scorex.db.{LDBFactory, StoreRegistry}
import scorex.util.bytesToId

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Isolates chunk reception after manifest selection: the manifest and tracker requests are registered
  * directly. Serialized peer messages enter the production synchronizer, which persists chunks and
  * requests initialization from its holder. This does not exercise wire manifest negotiation or state import.
  */
class DigestSnapshotNetworkSpecification extends ErgoCorePropertyTest with FileUtils {

  private def parsedSettings(directory: File, mode: StateType): ErgoSettings = {
    Files.createDirectories(directory.toPath.resolve("wallet/keystore"))
    val config = ConfigFactory.parseString(
      s"""ergo.node.stateType = "${mode.stateTypeName}"
         |ergo.node.utxo.utxoBootstrap = true
         |ergo.node.nipopow.nipopowBootstrap = false
         |ergo.node.verifyTransactions = true
         |ergo.node.mining = false
         |ergo.node.extraIndex = false
         |ergo.chain.voting.votingLength = 20
         |scorex.network.syncInterval = 1h
         |""".stripMargin)
      .withValue("ergo.directory", ConfigValueFactory.fromAnyRef(directory.getAbsolutePath))
      .withValue("ergo.wallet.secretStorage.secretDir",
        ConfigValueFactory.fromAnyRef(new File(directory, "wallet/keystore").getAbsolutePath))
      .withFallback(ConfigFactory.load()).resolve()
    val parsed = ErgoSettingsReader.fromConfig(config)
    parsed.nodeSettings.stateType shouldBe mode
    parsed.nodeSettings.utxoSettings.utxoBootstrap shouldBe true
    parsed.copy(chainSettings = parsed.chainSettings.copy(
      powScheme = new DefaultFakePowScheme(parsed.chainSettings.powScheme.k, parsed.chainSettings.powScheme.n)))
  }

  private def closeOwnedStores(directory: File): Unit = {
    val root = directory.getCanonicalFile.toPath
    val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
    registry.lock.writeLock().lock()
    try {
      registry.map.toVector.collect {
        case (path, db) if path.getCanonicalFile.toPath.startsWith(root) => db
      }.foreach(_.close())
      registry.map.keys.exists(_.getCanonicalFile.toPath.startsWith(root)) shouldBe false
    } finally registry.lock.writeLock().unlock()
  }

  private case class SnapshotBytes(manifest: Array[Byte], chunks: Vector[Array[Byte]], root: ADDigest)

  // Reuse only immutable serialized producer output; actor and database state are isolated per case.
  private lazy val snapshot: SnapshotBytes = {
    val directory = Files.createTempDirectory("snapshot-network-source-").toFile
    val settings = parsedSettings(directory, StateType.Utxo)
    try {
      val stateDir = new File(directory, "state")
      Files.createDirectories(stateDir.toPath)
      val boxes = boxesHolderGenOfSize(32 * 1024).sample.get
      val source = UtxoState.fromBoxHolder(boxes, None, stateDir, settings, parameters)
      ManifestSerializer.MainnetManifestDepth shouldBe 14
      source.dumpSnapshot(19, source.rootDigest.dropRight(1)).get
      val id = source.snapshotsDb.readSnapshotsInfo.availableManifests(19)
      val bytes = source.snapshotsDb.readManifestBytes(id).get
      val manifest = ManifestSerializer.defaultSerializer.parseBytes(bytes)
      manifest.subtreesIds should not be empty
      val (rootHash, height) = splitDigest(source.rootDigest)
      manifest.verify(Digest32 @@ rootHash, height) shouldBe true
      val chunks = manifest.subtreesIds.map { subtreeId =>
        val chunk = source.snapshotsDb.readSubtreeBytes(subtreeId).get
        SubtreeSerializer.parseBytes(chunk).verify(subtreeId) shouldBe true
        chunk
      }.toVector
      info(s"Production-depth snapshot has ${chunks.size} nonempty subtree messages")
      SnapshotBytes(bytes, chunks, source.rootDigest)
    } finally {
      closeOwnedStores(directory)
      deleteRecursive(directory)
    }
  }

  Seq((StateType.Utxo, "requested"), (StateType.Digest, "requested"),
    (StateType.Digest, "unsolicited"), (StateType.Digest, "malformed")).foreach { case (mode, input) =>
    property(s"${mode.stateTypeName} snapshot chunk receive handles $input peer messages") {
      val fixture = snapshot
      val directory = Files.createTempDirectory("snapshot-network-receive-").toFile
      val settings = parsedSettings(directory, mode)
      implicit val system: ActorSystem = ActorSystem()
      implicit val ec: scala.concurrent.ExecutionContext = system.dispatcher
      try {
        var history = ErgoHistory.readOrGenerate(settings)(null)
        val generated = genHeaderChain(19, history, diffBitsOpt = None, useRealTs = false).headers
        // Canonical accepted headers identify the snapshot root, without reconstructing a full-block chain.
        val headers = generated.dropRight(1) :+ generated.last.copy(stateRoot = fixture.root)
        headers.foreach(header => history = history.append(header).get._1)
        val anchor = headers.last
        history.bestHeaderAtHeight(19).map(_.id) shouldBe Some(anchor.id)
        history.bestFullBlockOpt shouldBe None
        val state = ErgoState.readOrGenerate(settings)
        state.getClass.getSimpleName shouldBe (if (mode == StateType.Digest) "DigestState" else "UtxoState")
        val pool = ErgoMemPool.empty(settings)
        val network = TestProbe()
        val holder = TestProbe()
        val senderProbe = TestProbe()
        val peer = connectedPeerGen(TestProbe().ref).sample.get
        val tracker = DeliveryTracker.empty(settings)
        val manifest = ManifestSerializer.defaultSerializer.parseBytes(fixture.manifest)
        history.registerManifestToDownload(manifest, 19, Seq.empty)
        val chunkIds = history.getChunkIdsToDownload(manifest.subtreesIds.size)
        chunkIds.size shouldBe fixture.chunks.size
        if (input != "unsolicited") {
          chunkIds.foreach { id =>
            tracker.setRequested(UtxoSnapshotChunkTypeId.value, bytesToId(id), peer) { _ =>
              Cancellable.alreadyCancelled
            }
          }
        }
        val initialPlan = history.utxoSetSnapshotDownloadPlan().get
        initialPlan.fullyDownloaded shouldBe false
        history.downloadedChunksIterator().toVector shouldBe empty
        val synchronizer = ErgoNodeViewSynchronizer.make(
          holder.ref, ErgoSyncInfoMessageSpec, settings,
          ErgoSyncTracker(settings.scorexSettings.network), tracker)(system, ec)(network.ref)
        holder.expectMsgType[GetNodeViewChanges](10.seconds)
        // Same-sender ordering initializes the real reader mode before its first chunk message.
        senderProbe.send(synchronizer, ChangedState(state))
        senderProbe.send(synchronizer, ChangedHistory(history))
        senderProbe.send(synchronizer, ChangedMempool(pool))

        val chunksToSend = input match {
          case "requested" => fixture.chunks
          case "unsolicited" => fixture.chunks.take(1)
          case "malformed" => Vector(Array.emptyByteArray)
        }
        chunksToSend.foreach { chunk =>
          senderProbe.send(synchronizer,
            Message(UtxoSnapshotChunkSpec, Left(UtxoSnapshotChunkSpec.toBytes(chunk)), Some(peer)))
        }
        if (input == "requested") {
          holder.expectMsg(20.seconds, InitStateFromSnapshot(19, anchor.id))
          val completed = history.utxoSetSnapshotDownloadPlan().get
          completed.fullyDownloaded shouldBe true
          completed.downloadingChunks shouldBe 0
          history.downloadedChunksIterator().map(chunk => Algos.encode(chunk.id)).toSet shouldBe
            chunkIds.map(Algos.encode(_)).toSet
          chunkIds.foreach(id => tracker.getRequestedInfo(UtxoSnapshotChunkTypeId.value, bytesToId(id)) shouldBe None)
          network.expectNoMessage(200.millis)
          senderProbe.send(synchronizer,
            Message(UtxoSnapshotChunkSpec, Left(UtxoSnapshotChunkSpec.toBytes(fixture.chunks.head)), Some(peer)))
          network.expectMsgType[PenalizePeer](10.seconds).penaltyType shouldBe PenaltyType.SpamPenalty
          history.utxoSetSnapshotDownloadPlan().get shouldBe completed
          holder.expectNoMessage(200.millis)
        } else {
          val penalty = network.expectMsgType[PenalizePeer](10.seconds)
          penalty.address shouldBe peer.connectionId.remoteAddress
          penalty.penaltyType shouldBe (if (input == "unsolicited") PenaltyType.SpamPenalty else PenaltyType.MisbehaviorPenalty)
          history.utxoSetSnapshotDownloadPlan().get shouldBe initialPlan
          history.downloadedChunksIterator().toVector shouldBe empty
          holder.expectNoMessage(200.millis)
          if (input == "malformed") {
            chunkIds.foreach(id => tracker.getRequestedInfo(UtxoSnapshotChunkTypeId.value, bytesToId(id)).isDefined shouldBe true)
          }
        }
        if (mode == StateType.Digest) {
          senderProbe.send(synchronizer,
            Message(GetManifestSpec, Left(GetManifestSpec.toBytes(manifest.id)), Some(peer)))
          senderProbe.send(synchronizer,
            Message(GetUtxoSnapshotChunkSpec, Left(GetUtxoSnapshotChunkSpec.toBytes(chunkIds.head)), Some(peer)))
          network.expectNoMessage(200.millis)
          holder.expectNoMessage(200.millis)
        }
        // The holder probe observes the initialization request but never performs the state import.
        history.isUtxoSnapshotApplied shouldBe false
      } finally {
        Await.result(system.terminate(), 30.seconds)
        closeOwnedStores(directory)
        deleteRecursive(directory)
      }
    }
  }
}
