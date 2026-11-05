package scorex.db

import java.io.File
import java.nio.ByteBuffer
import java.util.concurrent.locks.ReentrantReadWriteLock

import org.iq80.leveldb.{DB, Options, ReadOptions}
import scorex.crypto.hash.Blake2b256
import scorex.db.LDBVersionedStore.SnapshotReadInterface
import scorex.db.LDBVersionedStoreJournal.{Change, Metadata, Plan, Version}
import scorex.util.ScorexLogging

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Versioned storage with a durable redo journal coordinating main data and undo history.
  *
  * An ambiguous write result makes this instance unavailable. Close it and reopen the store,
  * then rebuild its consumers: recovery may complete an operation whose caller received Failure.
  * Backups must include ldb_main, ldb_undo and ldb_journal together. After journal adoption,
  * writes by older binaries are unsupported even without pending recovery: they cannot update
  * the committed journal metadata. Legacy data and undo record encodings are unchanged.
  * Recovery guards cover the store API; direct access through the raw db handle bypasses them.
  *
  * @param dir folder containing the databases
  * @param initialKeepVersions rollback depth for this instance; zero retains only the current version
  */
class LDBVersionedStore private[db](protected val dir: File,
                                  val initialKeepVersions: Int,
                                  openDatabase: (File, String) => DB)
  extends KVStoreReader with ScorexLogging {

  def this(dir: File, initialKeepVersions: Int) =
    this(dir, initialKeepVersions, LDBVersionedStore.openDatabase)

  type VersionID = Array[Byte]
  type LSN = Long

  require(initialKeepVersions >= 0, "Negative keepVersions")

  private val last_version_key = Blake2b256("last_version")
  private var keepVersions: Int = initialKeepVersions
  override val lock = new ReentrantReadWriteLock()
  private var recoveryFailure: Option[Throwable] = None
  private var closed = false

  private val databases = {
    val opened = ArrayBuffer.empty[DB]
    try {
      Seq("ldb_main", "ldb_undo", "ldb_journal").foreach { name => opened += openDatabase(dir, name) }
      opened.toVector
    } catch {
      case t: Throwable =>
        opened.reverse.foreach(database => closeAfterFailure(database, t))
        throw t
    }
  }
  override val db: DB = databases(0)
  private val undo: DB = databases(1)
  private val journal = new LDBVersionedStoreJournal(databases(2), db, undo)
  private var metadata: Metadata = try journal.initialize(readLegacyMetadata()) catch {
    case t: Throwable =>
      databases.reverse.foreach(database => closeAfterFailure(database, t))
      throw t
  }

  private def closeAfterFailure(database: DB, failure: Throwable): Unit = try database.close() catch {
    case NonFatal(closeError) => if (closeError ne failure) failure.addSuppressed(closeError)
  }

  override protected def ensureReadable(): Unit = {
    require(!closed, "Versioned store is closed")
    recoveryFailure.foreach { cause =>
      throw new IllegalStateException("Versioned store requires close and reopen before further access", cause)
    }
  }

  private def readLocked[T](body: => T): T = {
    lock.readLock().lock()
    try {
      ensureReadable()
      body
    } finally lock.readLock().unlock()
  }

  private def writeLocked[T](body: => T): T = {
    lock.writeLock().lock()
    try {
      ensureReadable()
      body
    } finally lock.writeLock().unlock()
  }

  /** Return the previous threshold only after any required pruning has committed. */
  def setKeepVersions(newKeepVersions: Int): Int = writeLocked {
    require(newKeepVersions >= 0, "Negative keepVersions")
    val previous = keepVersions
    if (newKeepVersions < previous) commit(pruned(emptyPlan, newKeepVersions))
    keepVersions = newKeepVersions
    previous
  }

  def getKeepVersions: Int = readLocked(keepVersions)

  def apply(key: K): V = getOrElse(key, throw new NoSuchElementException())

  def get(keys: Iterable[K], consumer: (K, Option[V]) => Unit): Unit = {
    readLocked(())
    keys.foreach(key => consumer(key, get(key)))
  }

  def processAll(consumer: (K, V) => Unit): Unit = readLocked {
    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      while (iterator.hasNext) {
        val entry = iterator.next()
        consumer(entry.getKey, entry.getValue)
      }
    } finally iterator.close()
  }

  private def decodeLSN(bytes: Array[Byte]): LSN = ~ByteBuffer.wrap(bytes).getLong

  private def encodeLSN(lsn: LSN): Array[Byte] = ByteBuffer.allocate(8).putLong(~lsn).array()

  def lastVersionID: Option[VersionID] = readLocked(metadata.versions.lastOption.map(_.id.clone()))

  def versionIdExists(versionID: VersionID): Boolean =
    readLocked(metadata.versions.exists(_.id.sameElements(versionID)))

  /** Adoption preserves the legacy reader's available history; it cannot certify earlier writes. */
  private def readLegacyMetadata(): Metadata = {
    val descending = ArrayBuffer.empty[Version]
    var lastLsn = 0L
    val iterator = undo.iterator()
    try {
      iterator.seekToFirst()
      while (iterator.hasNext) {
        val entry = iterator.next()
        val currentLsn = decodeLSN(entry.getKey)
        val version = deserializeUndo(entry.getValue).versionID
        if (lastLsn == 0L) lastLsn = currentLsn
        if (descending.lastOption.exists(_.id.sameElements(version))) {
          descending(descending.size - 1) = Version(version, currentLsn)
        } else descending += Version(version, currentLsn)
      }
    } finally iterator.close()
    val versions = if (descending.nonEmpty) descending.reverse.toVector else {
      Option(db.get(last_version_key)).map(id => Vector(Version(id, 0L))).getOrElse(Vector.empty)
    }
    Metadata(0L, lastLsn, versions)
  }

  case class Undo(versionID: VersionID, key: Array[Byte], value: Array[Byte])

  private def serializeUndo(versionID: VersionID, key: Array[Byte], value: Array[Byte]): Array[Byte] = {
    val valueSize = if (value != null) value.length else 0
    val versionSize = versionID.length
    val keySize = key.length
    require(versionSize <= 0xFF && keySize <= 0xFF)
    val packed = new Array[Byte](2 + versionSize + keySize + valueSize)
    packed(0) = versionSize.toByte
    packed(1) = keySize.toByte
    Array.copy(versionID, 0, packed, 2, versionSize)
    Array.copy(key, 0, packed, 2 + versionSize, keySize)
    if (value != null) Array.copy(value, 0, packed, 2 + versionSize + keySize, valueSize)
    packed
  }

  private def deserializeUndo(bytes: Array[Byte]): Undo = {
    val versionSize = bytes(0) & 0xFF
    val keySize = bytes(1) & 0xFF
    val valueSize = bytes.length - versionSize - keySize - 2
    val versionID = bytes.slice(2, 2 + versionSize)
    val key = bytes.slice(2 + versionSize, 2 + versionSize + keySize)
    val value = if (valueSize == 0) null else bytes.slice(2 + versionSize + keySize, bytes.length)
    Undo(versionID, key, value)
  }

  private def emptyPlan: Plan = Plan(
    metadata.copy(transaction = Math.addExact(metadata.transaction, 1L)), Vector.empty, Vector.empty)

  /** The only publication point for a changed data/version/history state. */
  private def commit(plan: Plan): Unit = {
    val encoded = journal.encoded(plan)
    try {
      journal.prepare(encoded)
      journal.applyParticipants(plan)
      journal.complete(plan)
      metadata = plan.result
    } catch {
      case t: Throwable =>
        recoveryFailure = Some(t)
        throw t
    }
  }

  /** Write a recoverable batch. A Failure after a write attempt requires reopening this instance. */
  def update(versionID: VersionID,
             toRemove: TraversableOnce[Array[Byte]],
             toUpdate: TraversableOnce[(Array[Byte], Array[Byte])]): Try[Unit] = Try(writeLocked {
    val version = versionID.clone()
    val mainChanges = Vector.newBuilder[Change]
    val undoChanges = Vector.newBuilder[Change]
    var nextLsn = metadata.lsn
    def recordUndo(key: Array[Byte], value: Array[Byte]): Unit = {
      nextLsn = Math.addExact(nextLsn, 1L)
      undoChanges += Change(encodeLSN(nextLsn), Some(serializeUndo(version, key, value)))
    }
    toRemove.foreach { input =>
      val key = input.clone()
      mainChanges += Change(key, None)
      if (keepVersions > 0) Option(db.get(key)).foreach(value => recordUndo(key, value))
    }
    toUpdate.foreach { case (inputKey, inputValue) =>
      val key = inputKey.clone()
      val value = inputValue.clone()
      require(key.nonEmpty, "Empty keys are not allowed")
      if (keepVersions > 0) recordUndo(key, db.get(key))
      mainChanges += Change(key, Some(value))
    }

    val sameVersion = metadata.versions.lastOption.exists(_.id.sameElements(version))
    val nextVersions = if (keepVersions > 0) {
      if (nextLsn == metadata.lsn) recordUndo(Array.emptyByteArray, null)
      if (sameVersion) metadata.versions else metadata.versions :+ Version(version, metadata.lsn + 1L)
    } else {
      if (!sameVersion) mainChanges += Change(last_version_key, Some(version.clone()))
      Vector(Version(version, nextLsn))
    }
    val result = Metadata(Math.addExact(metadata.transaction, 1L), nextLsn, nextVersions)
    val plan = Plan(result, mainChanges.result(), undoChanges.result())
    commit(if (keepVersions == 0 || !sameVersion) pruned(plan, keepVersions) else plan)
  })

  def insert(versionID: VersionID, toInsert: Seq[(K, V)]): Try[Unit] = update(versionID, Seq.empty, toInsert)

  def remove(versionID: VersionID, toRemove: Seq[K]): Try[Unit] = update(versionID, toRemove, Seq.empty)

  /** Retain count predecessors plus the current version, including the oldest rollback anchor. */
  private def pruned(plan: Plan, count: Int): Plan = {
    require(count >= 0, "Negative retention count")
    val versions = plan.result.versions
    if (versions.isEmpty || versions.size.toLong <= count.toLong) plan else {
      val drop = math.max(0, versions.size - count - 1)
      val cutoff = if (drop + 1 < versions.size) versions(drop + 1).firstLsn else plan.result.lsn + 1L
      val deletes = Vector.newBuilder[Change]
      if (cutoff > 0) {
        val iterator = undo.iterator()
        try {
          iterator.seek(encodeLSN(cutoff - 1L))
          while (iterator.hasNext) deletes += Change(iterator.next().getKey.clone(), None)
        } finally iterator.close()
      }
      plan.undo.foreach { change =>
        if (decodeLSN(change.key) < cutoff) deletes += Change(change.key, None)
      }
      val main = if (count == 0) plan.main :+ Change(last_version_key, Some(versions.last.id.clone())) else plan.main
      plan.copy(result = plan.result.copy(versions = versions.drop(drop)), main = main, undo = plan.undo ++ deletes.result())
    }
  }

  def clean(count: Int): Unit = writeLocked {
    commit(pruned(emptyPlan, count))
    Seq(undo, db).foreach { database =>
      Try(database.resumeCompactions()).failed.foreach { error =>
        log.warn("History retention committed but compaction could not resume", error)
      }
    }
  }

  def cleanStop(): Unit = writeLocked {
    undo.suspendCompactions()
    db.suspendCompactions()
  }

  override def close(): Unit = {
    lock.writeLock().lock()
    try {
      if (!closed) {
        closed = true
        var failure: Throwable = null
        databases.reverse.foreach { database =>
          try database.close() catch {
            case NonFatal(error) =>
              if (failure == null) failure = error else if (failure ne error) failure.addSuppressed(error)
          }
        }
        if (failure != null) throw failure
      }
    } finally lock.writeLock().unlock()
  }

  def rollbackTo(versionID: VersionID): Try[Unit] = Try(writeLocked {
    val index = metadata.versions.indexWhere(_.id.sameElements(versionID))
    if (index < 0) throw new NoSuchElementException("versionID not found, can not rollback")
    if (index < metadata.versions.size - 1) {
      val boundary = metadata.versions(index + 1).firstLsn
      val mainChanges = Vector.newBuilder[Change]
      val undoChanges = Vector.newBuilder[Change]
      var count = 0L
      val iterator = undo.iterator()
      try {
        iterator.seekToFirst()
        while (iterator.hasNext && decodeLSN(iterator.peekNext().getKey) >= boundary) {
          val entry = iterator.next()
          val record = deserializeUndo(entry.getValue)
          if (record.key.nonEmpty) mainChanges += Change(record.key, Option(record.value))
          undoChanges += Change(entry.getKey.clone(), None)
          count += 1L
        }
      } finally iterator.close()
      require(count == metadata.lsn - boundary + 1L, "Incomplete retained rollback history")
      val result = Metadata(Math.addExact(metadata.transaction, 1L), boundary - 1L, metadata.versions.take(index + 1))
      // Keep the legacy fallback marker consistent when it exists; do not add it to ordinary main data.
      if (db.get(last_version_key) != null) mainChanges += Change(last_version_key, Some(versionID.clone()))
      commit(Plan(result, mainChanges.result(), undoChanges.result()))
    }
  })

  def rollbackVersions(): Iterable[VersionID] =
    readLocked(metadata.versions.reverse.map(_.id.clone()))

  /** Process a committed snapshot; already acquired snapshots remain valid during later writes. */
  def processSnapshot[T](logic: SnapshotReadInterface => T): Try[T] = {
    val ro = new ReadOptions()
    try {
      lock.writeLock().lock()
      val snapshot = try {
        ensureReadable()
        db.getSnapshot
      } finally {
        lock.writeLock().unlock()
      }
      var processingFailure: Throwable = null
      try {
        ro.snapshot(snapshot)
        object readInterface extends SnapshotReadInterface {
          def get(key: Array[Byte]): Array[Byte] = db.get(key, ro)
        }
        Success(logic(readInterface))
      } catch {
        case t: Throwable =>
          processingFailure = t
          throw t
      } finally {
        try {
          snapshot.close()
        } catch {
          case NonFatal(t) if processingFailure != null =>
            if (t ne processingFailure) processingFailure.addSuppressed(t)
        }
      }
    } catch {
      case NonFatal(t) =>
        log.info("Error during snapshot processing: ", t)
        Failure(t)
    }
  }
}

object LDBVersionedStore {
  private[db] def openDatabase(dir: File, name: String): DB = {
    val options = new Options().createIfMissing(true).paranoidChecks(true)
    LDBFactory.factory.open(new File(dir, name), options)
  }

  trait SnapshotReadInterface {
    /** Returns the stored bytes, or null if the key is absent. */
    def get(key: Array[Byte]): Array[Byte]
  }
}
