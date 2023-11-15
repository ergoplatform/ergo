package org.ergoplatform.nodeView.state

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.nodeView.state.UtxoState.{ManifestId, SubtreeId}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.serialization.ManifestSerializer
import scorex.crypto.authds.avltree.batch.VersionedLDBAVLStorage
import scorex.crypto.hash.Digest32
import scorex.db.{LDBFactory, LDBKVStore}
import scorex.util.ScorexLogging
import scorex.util.encode.Base16

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/**
  * Interface for a (non-versioned) database storing UTXO set snapshots and metadata about them
  */
class SnapshotsDb(store: LDBKVStore) extends ScorexLogging {

  private val snapshotInfoKey: Array[Byte] = Array.fill(32)(0: Byte)

  // helper method to write information about store UTXO set snapshots into the database
  /// private[nodeView] as used in some tests
  private[nodeView] def writeSnapshotsInfo(snapshotsInfo: SnapshotsInfo): Try[Unit] = {
    store.insert(snapshotInfoKey, SnapshotsInfoSerializer.toBytes(snapshotsInfo))
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
    val manifestSerializer = ManifestSerializer.defaultSerializer

    if (manifests.size > toStore) {

      val lastManifestBytesOpt = store.get(manifests.last._2)
      val lastManifestSubtrees =
        lastManifestBytesOpt
          .flatMap(bs => manifestSerializer.parseBytesTry(bs).toOption)
          .map(_.subtreesIds)
          .getOrElse(ArrayBuffer.empty)
          .map(Algos.encode)
          .toSet

      val toPrune = manifests.dropRight(toStore)
      val toLeave = manifests.takeRight(toStore)

      toPrune.foreach { case (h, manifestId) =>
        val pt0 = System.currentTimeMillis()
        val keysToRemove: Array[Array[Byte]] = store.get(manifestId) match {
          case Some(manifestBytes) =>
            manifestSerializer.parseBytesTry(manifestBytes) match {
              case Success(m) =>
                val keys = mutable.ArrayBuilder.make[Array[Byte]]()
                val subtrees = m.subtreesIds
                keys.sizeHint(subtrees.size + 1)
                keys += manifestId
                // filter out subtrees which are the same in the latest version of tree snapshot
                subtrees.foreach { subtreeId =>
                  if (!lastManifestSubtrees.contains(Algos.encode(subtreeId))) {
                    keys += subtreeId
                  }
                }
                keys.result()
              case Failure(e) =>
                log.error(s"Can't parse manifest ${Base16.encode(manifestId)} :", e)
                Array.empty
            }
          case None =>
            log.error(s"Manifest ${Base16.encode(manifestId)} not found when should be pruned")
            Array.empty
        }
        store.remove(keysToRemove)
        val pt = System.currentTimeMillis()
        log.info(s"Pruning snapshot at height $h done in ${pt - pt0} ms.")
      }

      val updInfo = new SnapshotsInfo(toLeave.toMap)
      writeSnapshotsInfo(updInfo)

      log.info("Snapshots pruning finished")
    } else {
      log.debug("No snapshots to prune")
    }
  }

  /**
    * Lazily read current UTXO set snapshot from versioned AVL+ tree database and store it in this snapshots database
    *
    * @param pullFrom         - versioned AVL+ tree database to pull snapshot from
    * @param height           - height of a block snapshot is corresponding to
    * @param expectedRootHash - expected tree root hash in `pullFrom` database
    * @return - id of the snapshot (root hash of its authenticating AVL+ tree),
    *         or error happened during read-write process
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
    *
    * @param id - manifest id
    */
  def readManifestBytes(id: ManifestId): Option[Array[Byte]] = {
    store.get(id)
  }

  /**
    * Read subtree bytes without deserializing it. Useful when subtree is to be sent over the wir
    *
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
