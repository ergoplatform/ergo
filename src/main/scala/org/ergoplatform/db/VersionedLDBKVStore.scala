package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.{Algos, Constants}
import org.iq80.leveldb.{DB, ReadOptions}

import scala.util.{Failure, Success, Try}

/**
  * A LevelDB wrapper providing additional versioning layer along with a convenient db interface.
  */
final class VersionedLDBKVStore(protected val db: DB) extends KVStore {

  private val versionsKey = ByteString("versions")

  def update(toInsert: Seq[(ByteString, ByteString)], toRemove: Seq[ByteString])
            (version: ByteString): Unit = {
    require(version.size == Constants.HashLength, "Illegal version id size")
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    val (insertedKeys, altered) = toInsert.foldLeft(
      Seq.empty[ByteString], Seq.empty[(ByteString, (ByteString, ByteString))]) {
      case ((insertedAcc, alteredAcc), (k, v)) =>
        Option(db.get(k.toArray, ro))
          .map { oldValue =>
            insertedAcc -> ((k -> (ByteString(oldValue) -> v)) +: alteredAcc)
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

  def rollbackTo(version: ByteString): Try[Unit] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    Option(db.get(versionsKey.toArray)) match {
      case Some(bytes) =>
        val batch = db.createWriteBatch()
        try {
          bytes.grouped(Constants.HashLength)
            .takeWhile(ByteString(_) != version) // select all versions above targeted one
            .foldLeft(Seq.empty[(Array[Byte], ChangeSet)]) { case (acc, verId) =>
              val changeSetOpt = Option(db.get(verId, ro)).flatMap { changeSetBytes =>
                ChangeSetSerializer.parseBytesTry(changeSetBytes).toOption
              }
              require(changeSetOpt.isDefined, "Inconsistent versioned storage state")
              acc ++ changeSetOpt.toSeq.map(verId -> _) // prepend newer version
            }
            .foreach { case (verId, changeSet) => // revert all changes (from newest version to the targeted one)
              changeSet.insertedKeys.foreach(k => batch.delete(k.toArray))
              changeSet.removed.foreach { case (k, v) =>
                batch.put(k.toArray, v.toArray)
              }
              changeSet.altered.foreach { case (k, (oldV, _)) =>
                batch.put(k.toArray, oldV.toArray)
              }
              batch.delete(verId)
            }

          val updatedVersions = bytes
            .grouped(Constants.HashLength)
            .map(ByteString.apply)
            .dropWhile(_ != version)
            .reduce(_ ++ _)

          batch.put(versionsKey.toArray, updatedVersions.toArray)

          db.write(batch)
          Success(())
        } finally {
          batch.close()
          ro.snapshot().close()
        }
      case None =>
        Failure(new Exception(s"Version ${Algos.encode(version.toArray)} not found"))
    }
  }

}
