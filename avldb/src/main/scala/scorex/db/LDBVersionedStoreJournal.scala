package scorex.db

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import org.iq80.leveldb.{DB, WriteOptions}
import scorex.util.ScorexLogging

import scala.util.control.NonFatal

/** Private redo journal. Application keys and the legacy undo encoding remain unchanged. */
private[db] final class LDBVersionedStoreJournal(journal: DB, main: DB, undo: DB) extends ScorexLogging {
  import LDBVersionedStoreJournal._

  private val synchronous = new WriteOptions().sync(true)

  def initialize(legacy: => Metadata): Metadata = {
    val committed = Option(journal.get(CommittedKey)).map(decodeMetadata).getOrElse {
      require(journal.get(PendingKey) == null, "Pending recovery has no committed predecessor")
      val initial = legacy
      write(journal, Vector(Change(CommittedKey, Some(encodeMetadata(initial)))))
      initial
    }
    Option(journal.get(PendingKey)) match {
      case Some(bytes) =>
        val pending = decodePlan(bytes)
        require(pending.result.transaction == Math.addExact(committed.transaction, 1L),
          "Pending recovery does not follow committed metadata")
        applyParticipants(pending)
        complete(pending)
        pending.result
      case None => committed
    }
  }

  /** Serialize before the first write, so encoding errors are ordinary preflight failures. */
  def encoded(plan: Plan): Array[Byte] = encodePlan(plan)

  def prepare(bytes: Array[Byte]): Unit =
    write(journal, Vector(Change(PendingKey, Some(bytes))))

  def applyParticipants(plan: Plan): Unit = {
    write(main, plan.main)
    write(undo, plan.undo)
  }

  def complete(plan: Plan): Unit = write(journal, Vector(
    Change(CommittedKey, Some(encodeMetadata(plan.result))), Change(PendingKey, None)))

  private def write(database: DB, changes: Vector[Change]): Unit = {
    val batch = database.createWriteBatch()
    var failure: Throwable = null
    try {
      changes.foreach { change =>
        change.value match {
          case Some(value) => batch.put(change.key, value)
          case None => batch.delete(change.key)
        }
      }
      database.write(batch, synchronous)
    } catch {
      case t: Throwable =>
        failure = t
        throw t
    } finally {
      try batch.close() catch {
        case NonFatal(closeError) if failure != null =>
          if (closeError ne failure) failure.addSuppressed(closeError)
        case NonFatal(closeError) =>
          // The write completed. Cleanup must not turn a committed operation into a failed one.
          log.warn("Failed to close completed versioned-store write batch", closeError)
      }
    }
  }
}

private[db] object LDBVersionedStoreJournal {
  private val Format = 1
  private val CommittedKey = Array(0.toByte)
  private val PendingKey = Array(1.toByte)

  final case class Version(id: Array[Byte], firstLsn: Long)
  final case class Metadata(transaction: Long, lsn: Long, versions: Vector[Version])
  final case class Change(key: Array[Byte], value: Option[Array[Byte]])
  final case class Plan(result: Metadata, main: Vector[Change], undo: Vector[Change])

  private def encode(write: DataOutputStream => Unit): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val out = new DataOutputStream(bytes)
    out.writeInt(Format)
    write(out)
    out.flush()
    bytes.toByteArray
  }

  private def decode[T](bytes: Array[Byte])(read: DataInputStream => T): T = {
    val in = new DataInputStream(new ByteArrayInputStream(bytes))
    require(in.readInt() == Format, "Unsupported versioned-store journal format")
    val result = read(in)
    require(in.available() == 0, "Trailing versioned-store journal bytes")
    result
  }

  private def writeBytes(out: DataOutputStream, bytes: Array[Byte]): Unit = {
    out.writeInt(bytes.length)
    out.write(bytes)
  }

  private def readBytes(in: DataInputStream): Array[Byte] = {
    val length = in.readInt()
    require(length >= 0 && length <= in.available(), "Incomplete versioned-store journal field")
    val bytes = new Array[Byte](length)
    in.readFully(bytes)
    bytes
  }

  private def writeMetadata(out: DataOutputStream, metadata: Metadata): Unit = {
    out.writeLong(metadata.transaction)
    out.writeLong(metadata.lsn)
    out.writeInt(metadata.versions.size)
    metadata.versions.foreach { version =>
      writeBytes(out, version.id)
      out.writeLong(version.firstLsn)
    }
  }

  private def readMetadata(in: DataInputStream): Metadata = {
    val transaction = in.readLong()
    val lsn = in.readLong()
    val count = in.readInt()
    require(transaction >= 0 && lsn >= 0, "Negative versioned-store journal sequence")
    require(count >= 0 && count <= in.available() / 12, "Incomplete versioned-store version list")
    val versions = Vector.fill(count) {
      val id = readBytes(in)
      val firstLsn = in.readLong()
      require(firstLsn >= 0 && firstLsn <= Math.addExact(lsn, 1L), "Invalid versioned-store LSN")
      Version(id, firstLsn)
    }
    require(versions.map(_.firstLsn).sliding(2).forall(pair => pair.size < 2 || pair.head <= pair.last),
      "Unordered versioned-store LSNs")
    Metadata(transaction, lsn, versions)
  }

  private def writeChanges(out: DataOutputStream, changes: Vector[Change]): Unit = {
    out.writeInt(changes.size)
    changes.foreach { change =>
      writeBytes(out, change.key)
      out.writeBoolean(change.value.isDefined)
      change.value.foreach(writeBytes(out, _))
    }
  }

  private def readChanges(in: DataInputStream): Vector[Change] = {
    val count = in.readInt()
    require(count >= 0 && count <= in.available() / 5, "Incomplete versioned-store change list")
    Vector.fill(count) {
      val key = readBytes(in)
      val value = if (in.readBoolean()) Some(readBytes(in)) else None
      Change(key, value)
    }
  }

  private def encodeMetadata(metadata: Metadata): Array[Byte] = encode(writeMetadata(_, metadata))
  private def decodeMetadata(bytes: Array[Byte]): Metadata = decode(bytes)(readMetadata)
  private def encodePlan(plan: Plan): Array[Byte] = encode { out =>
    writeMetadata(out, plan.result)
    writeChanges(out, plan.main)
    writeChanges(out, plan.undo)
  }
  private def decodePlan(bytes: Array[Byte]): Plan = decode(bytes) { in =>
    Plan(readMetadata(in), readChanges(in), readChanges(in))
  }
}
