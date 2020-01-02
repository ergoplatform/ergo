package org.ergoplatform.db

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.settings.{Algos, Constants}
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.db.KVStore

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

//todo: old class, now used in benchmarks only, remove it after 3.2.0!
/**
  * A LevelDB wrapper providing additional versioning layer along with a convenient db interface.
  */
class VersionedLDBKVStore(protected val db: DB, keepVersions: Int) extends KVStore {

  import VersionedLDBKVStore.VersionId

  private val VersionPrefix = 0: Byte
  val VersionsKey: Array[Byte] = VersionPrefix +: Algos.hash("versions")

  val ChangeSetPrefix: Byte = 0x16

  /**
    * Performs versioned update.
    * @param toInsert - key, value pairs to be inserted/updated
    * @param toRemove - keys to be removed
    * @param version - version tag for the update
    */
  def update(toInsert: Seq[(K, V)], toRemove: Seq[K])(version: VersionId): Unit = {
    require(version.length == Constants.HashLength, "Illegal version id size")
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)

    require(Option(db.get(version, ro)).isEmpty, "Version id is already used")

    val insertedKeys = mutable.ArrayBuffer.empty[K]
    val altered = mutable.ArrayBuffer.empty[(K, V)]
    toInsert.foreach(x => Option(db.get(x._1, ro))
      .fold[Unit](insertedKeys += x._1)(oldValue => altered += (x._1 -> oldValue)))

    val removed = toRemove.flatMap { k =>
      Option(db.get(k, ro)).map(k -> _)
    }

    val changeSet = ChangeSet(insertedKeys, removed, altered)
    val (updatedVersions, versionsToShrink) = Option(db.get(VersionsKey, ro))
      .map(version ++ _) // newer version first
      .getOrElse(version)
      .splitAt(Constants.HashLength * keepVersions) // shrink old versions

    val versionIdsToShrink = versionsToShrink.grouped(Constants.HashLength)
    val batch = db.createWriteBatch()

    try {
      batch.put(VersionsKey, updatedVersions)
      versionIdsToShrink.foreach(batch.delete)
      batch.put(version, ChangeSetPrefix +: ChangeSetSerializer.toBytes(changeSet))
      toInsert.foreach { case (k, v) => batch.put(k, v) }
      toRemove.foreach(batch.delete)
      db.write(batch)
    } finally {
      batch.close()
      ro.snapshot().close()
    }
  }

  def insert(toInsert: Seq[(K, V)])(version: VersionId): Unit = update(toInsert, Seq.empty)(version)

  def remove(toRemove: Seq[K])(version: VersionId): Unit = update(Seq.empty, toRemove)(version)

  /**
    * Rolls storage state back to the specified checkpoint.
    * @param versionId - version id to roll back to
    */
  def rollbackTo(versionId: VersionId): Try[Unit] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    Option(db.get(VersionsKey)) match {
      case Some(bytes) =>
        val batch = db.createWriteBatch()
        try {
          val versionsToRollBack = bytes
            .grouped(Constants.HashLength)
            .takeWhile(ByteArrayWrapper(_) != ByteArrayWrapper(versionId))

          versionsToRollBack
            .foldLeft(Seq.empty[(Array[Byte], ChangeSet)]) { case (acc, verId) =>
              val changeSetOpt = Option(db.get(verId, ro)).flatMap { changeSetBytes =>
                ChangeSetSerializer.parseBytesTry(changeSetBytes.tail).toOption
              }
              require(changeSetOpt.isDefined, s"Inconsistent versioned storage state")
              acc ++ changeSetOpt.toSeq.map(verId -> _)
            }
            .foreach { case (verId, changeSet) => // revert all changes (from newest version to the targeted one)
              changeSet.insertedKeys.foreach(k => batch.delete(k))
              changeSet.removed.foreach { case (k, v) =>
                batch.put(k, v)
              }
              changeSet.altered.foreach { case (k, oldV) =>
                batch.put(k, oldV)
              }
              batch.delete(verId)
            }

          val wrappedVersionId = ByteArrayWrapper(versionId)
          val updatedVersions = bytes
            .grouped(Constants.HashLength)
            .map(ByteArrayWrapper.apply)
            .dropWhile(_ != wrappedVersionId)
            .foldLeft(Array.empty[Byte]) { case (acc, arr) =>
              acc ++ arr.data
            }

          versionsToRollBack.foreach(batch.delete) // eliminate rolled back versions
          batch.put(VersionsKey, updatedVersions)

          db.write(batch)
          Success(())
        } finally {
          batch.close()
          ro.snapshot().close()
        }
      case None =>
        Failure(new Exception(s"Version ${Algos.encode(versionId)} not found"))
    }
  }

  def versions: Seq[VersionId] = Option(db.get(VersionsKey))
    .toSeq
    .flatMap(_.grouped(Constants.HashLength))

  def versionIdExists(versionId: VersionId): Boolean =
    versions.exists(ByteArrayWrapper(_) == ByteArrayWrapper(versionId))

}

object VersionedLDBKVStore {

  type VersionId = Array[Byte]

}
