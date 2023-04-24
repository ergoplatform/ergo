package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.Algos.HF
import org.ergoplatform.settings.{ErgoAlgos, ErgoSettings}
import org.ergoplatform.wallet.Constants
import scorex.core.serialization.ManifestSerializer
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.crypto.authds.avltree.batch.serialization.BatchAVLProverSerializer
import scorex.crypto.hash.Digest32
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.ScorexLogging
import scorex.util.encode.Base16

import scala.util.{Failure, Success, Try}

/**
  * Interface for a (non-versioned) database storing UTXO set snapshots and metadata about them
  */
class SnapshotsDb(store: LDBKVStore) extends ScorexLogging {

  private val serializer = new BatchAVLProverSerializer[Digest32, HF]()(ErgoAlgos.hash)

  private val snapshotInfoKey: Array[Byte] = Array.fill(32)(0: Byte)

  // helper method to write information about store UTXO set snapshots into the database
  /// private[nodeView] as used in some tests
  private[nodeView] def writeSnapshotsInfo(snapshotsInfo: SnapshotsInfo): Try[Unit] = {
    store.insert(Array(snapshotInfoKey -> SnapshotsInfoSerializer.toBytes(snapshotsInfo)))
  }

  // helper method to read information about store UTXO set snapshots from the database
  /// private[nodeView] as used in some tests
  private[nodeView] def readSnapshotsInfo: SnapshotsInfo = {
    store.get(snapshotInfoKey).map(SnapshotsInfoSerializer.parseBytes).getOrElse(SnapshotsInfo.empty)
  }

  /**
    * Remove old snapshots in the database and metadata records about them, leaving only `toStore` most recent snapshots
    */
  def pruneSnapshots(toStore: Int): Unit = {
    log.info("Starting snapshots pruning")

    // sort manifests by height to prune oldest ones after
    val manifests = readSnapshotsInfo.availableManifests.toSeq.sortBy(_._1)

    val (toPrune, toLeave) = if (manifests.size > toStore) {
      val tp = manifests.dropRight(toStore)
      val tl = manifests.takeRight(toStore)
      tp -> tl
    } else {
      log.info("No snapshots to prune")
      return
    }

    toPrune.foreach { case (h, manifestId) =>
      log.info(s"Pruning snapshot at height $h")
      val keysToRemove: Array[Array[Byte]] = store.get(manifestId) match {
        case Some(manifestBytes) =>
          serializer.manifestFromBytes(manifestBytes, Constants.ModifierIdLength) match {
            case Success(m) =>
              (m.subtreesIds += manifestId).toArray // todo: more efficient construction
            case Failure(e) =>
              log.error(s"Can't parse manifest ${Base16.encode(manifestId)} :", e)
              Array.empty
          }
        case None =>
          log.error(s"Manifest ${Base16.encode(manifestId)} not found:")
          Array.empty
      }
      store.remove(keysToRemove)
    }

    val updInfo = new SnapshotsInfo(toLeave.toMap)
    writeSnapshotsInfo(updInfo)

    log.info("Snapshots pruning finished")
  }

  /**
    * Lazily read current UTXO set snapshot from versioned AVL+ tree database and store it in this snapshots database
    *
    * @param pullFrom - versioned AVL+ tree database to pull snapshot from
    * @param height - height of a block snapshot is corresponding to
    * @param expectedRootHash - expected tree root hash in `pullFrom` database
    * @return - id of the snapshot (root hash of its authenticating AVL+ tree),
    *           or error happened during read-write process
    */
  def writeSnapshot(pullFrom: VersionedLDBAVLStorage,
                    height: Height,
                    expectedRootHash: Array[Byte],
                    manifestDepth: Byte = ManifestSerializer.MainnetManifestDepth): Try[Array[Byte]] = {
    pullFrom.dumpSnapshot(store, manifestDepth, expectedRootHash).map { manifestId =>
      val si = readSnapshotsInfo.withNewManifest(height, Digest32 @@ manifestId)
      writeSnapshotsInfo(si)
      manifestId
    }
  }

  /**
    * Read manifest bytes without deserializing it. Useful when manifest is to be sent over the wir
    * @param id - manifest id
    */
  def readManifestBytes(id: ManifestId): Option[Array[Byte]] = {
    store.get(id)
  }

  /**
    * Read subtree bytes without deserializing it. Useful when subtree is to be sent over the wir
    * @param id - subtree id
    */
  def readSubtreeBytes(id: SubtreeId): Option[Array[Byte]] = {
    store.get(id)
  }

}

object SnapshotsDb {

  // internal method to open or init snapshots database in given folder
  // private[nodeView] to use it in tests also
  private[nodeView] def create(dir: String): SnapshotsDb = {
    val store = LDBFactory.createKvDb(dir)
    new SnapshotsDb(store)
  }

  /**
    * Read or create snapshots database in a folder defined by provided settings
    */
  def create(ergoSettings: ErgoSettings): SnapshotsDb = {
    val dir = s"${ergoSettings.directory}/snapshots"
    create(dir)
  }

}
