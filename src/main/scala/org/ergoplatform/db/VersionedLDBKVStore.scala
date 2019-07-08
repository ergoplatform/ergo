package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.{Algos, Constants}
import org.iq80.leveldb.{DB, ReadOptions}

import scala.util.{Failure, Success, Try}

/**
  * A LevelDB wrapper providing additional versioning layer along with a convenient db interface.
  */
final class VersionedLDBKVStore(protected val db: DB, keepVersions: Int) extends KVStore {

  import VersionedLDBKVStore.VersionId

  private val versionsKey = ByteString("versions")

  def update(toInsert: Seq[(K, V)], toRemove: Seq[K])(version: VersionId): Unit = {
    require(version.size == Constants.HashLength, "Illegal version id size")
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    require(Option(db.get(version.toArray, ro)).isEmpty, "Version id is already used")
    val (insertedKeys, altered) = toInsert.foldLeft(Seq.empty[K], Seq.empty[(K, V)]) {
      case ((insertedAcc, alteredAcc), (k, _)) =>
        Option(db.get(k.toArray, ro))
          .map { oldValue =>
            insertedAcc -> ((k -> ByteString(oldValue)) +: alteredAcc)
          }
          .getOrElse {
            (k +: insertedAcc) -> alteredAcc
          }
    }
    val removed = toRemove.flatMap { k =>
      Option(db.get(k.toArray, ro)).map(k -> ByteString(_))
    }
    val changeSet = ChangeSet(insertedKeys, removed, altered)
    val updatedVersions = Option(db.get(versionsKey.toArray, ro))
      .map(version.toArray ++ _) // newer version first
      .getOrElse(version.toArray)
      .take(Constants.HashLength * keepVersions) // shrink old versions
    val batch = db.createWriteBatch()
    try {
      batch.put(versionsKey.toArray, updatedVersions)
      batch.put(version.toArray, ChangeSetSerializer.toBytes(changeSet))
      toInsert.foreach { case (k, v) => batch.put(k.toArray, v.toArray) }
      toRemove.foreach(x => batch.delete(x.toArray))
      db.write(batch)
    } finally {
      batch.close()
      ro.snapshot().close()
    }
  }

  def insert(toInsert: Seq[(K, V)])(version: VersionId): Unit = update(toInsert, Seq.empty)(version)

  def remove(toRemove: Seq[K])(version: VersionId): Unit = update(Seq.empty, toRemove)(version)

  def rollbackTo(versionId: VersionId): Try[Unit] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    Option(db.get(versionsKey.toArray)) match {
      case Some(bytes) =>
        val batch = db.createWriteBatch()
        try {
          bytes.grouped(Constants.HashLength)
            .takeWhile(ByteString(_) != versionId) // select all versions above targeted one
            .foldLeft(Seq.empty[(Array[Byte], ChangeSet)]) { case (acc, verId) =>
              val changeSetOpt = Option(db.get(verId, ro)).flatMap { changeSetBytes =>
                ChangeSetSerializer.parseBytesTry(changeSetBytes).toOption
              }
              require(changeSetOpt.isDefined, s"Inconsistent versioned storage state")
              acc ++ changeSetOpt.toSeq.map(verId -> _)
            }
            .foreach { case (verId, changeSet) => // revert all changes (from newest version to the targeted one)
              changeSet.insertedKeys.foreach(k => batch.delete(k.toArray))
              changeSet.removed.foreach { case (k, v) =>
                batch.put(k.toArray, v.toArray)
              }
              changeSet.altered.foreach { case (k, oldV) =>
                batch.put(k.toArray, oldV.toArray)
              }
              batch.delete(verId)
            }

          val updatedVersions = bytes
            .grouped(Constants.HashLength)
            .map(ByteString.apply)
            .dropWhile(_ != versionId)
            .reduce(_ ++ _)

          batch.put(versionsKey.toArray, updatedVersions.toArray)

          db.write(batch)
          Success(())
        } finally {
          batch.close()
          ro.snapshot().close()
        }
      case None =>
        Failure(new Exception(s"Version ${Algos.encode(versionId.toArray)} not found"))
    }
  }

  def versions: Seq[VersionId] = Option(db.get(versionsKey.toArray))
    .toSeq
    .flatMap(_.grouped(Constants.HashLength))
    .map(ByteString.apply)

}

object VersionedLDBKVStore {
  type VersionId = ByteString
}
