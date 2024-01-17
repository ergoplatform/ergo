package scorex.db

import org.rocksdb.util.SizeUnit

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.rocksdb.{CompactionStyle, CompressionType, Options, ReadOptions, RocksDB, RocksIterator, Snapshot, WriteBatch, WriteOptions}
import scorex.util.ScorexLogging

import scala.collection.mutable

/**
  * Registry of opened LevelDB instances.
  * LevelDB prohibit access to the same storage file from more than one DB instance.
  * And ergo application (mostly tests) quite frequently doesn't not explicitly close
  * database and tries to reopen it.
  */
case class StoreRegistry() extends ScorexLogging {

  val lock = new ReentrantReadWriteLock()
  val map = new mutable.HashMap[File, RegisteredDB]

  /**
    * Decorator of LevelDB DB class which overrides close() methods and unlinks database from registry on close.
    * So if database was not explicitly closed, then next attempt to open database with the same path will
    * return existed instance instead of creating new one.
    */
  case class RegisteredDB(impl: RocksDB, path: File) {

    def get(key: Array[Byte]): Array[Byte] = impl.get(key)

    def get(key: Array[Byte], options: ReadOptions): Array[Byte] = impl.get(options, key)

    def iterator: RocksIterator = impl.newIterator()

    def iterator(options: ReadOptions): RocksIterator = impl.newIterator(options)

    def put(key: Array[Byte], value: Array[Byte]): Unit = impl.put(key, value)

    def delete(key: Array[Byte]): Unit = impl.delete(key)

    def write(batch: WriteBatch): Unit = impl.write(new WriteOptions(), batch)

    def write(batch: WriteBatch, options: WriteOptions): Unit = impl.write(options, batch)

    def createWriteBatch: WriteBatch = new WriteBatch()

    def put(key: Array[Byte], value: Array[Byte], options: WriteOptions): Unit = impl.put(options, key, value)

    def delete(key: Array[Byte], options: WriteOptions): Unit = impl.delete(options, key)

    def getSnapshot: Snapshot = impl.getSnapshot

    def getProperty(name: String): String = impl.getProperty(name)

    def compactRange(begin: Array[Byte], end: Array[Byte]): Unit = impl.compactRange(begin, end)

    def close(): Unit = {
      remove(path)
      impl.close()
    }
  }

  private def add(file: File, create: => RocksDB): RegisteredDB = {
    lock.writeLock().lock()
    try {
      map.getOrElseUpdate(file, RegisteredDB(create, file))
    } finally {
      lock.writeLock().unlock()
    }
  }

  private def remove(path: File): Option[RegisteredDB] = {
    lock.writeLock().lock()
    try {
      map.remove(path)
    } finally {
      lock.writeLock().unlock()
    }
  }

  def open(path: File, options: Options): RegisteredDB = {
    lock.writeLock().lock()
    try {
      add(path, LDBFactory.openDb(path, options))
    } catch {
      case x: Throwable =>
        log.error(s"Failed to initialize storage: $x. Please check that directory $path exists and is not used by some other active node")
        java.lang.System.exit(2)
        null
    } finally {
      lock.writeLock().unlock()
    }
  }

}

object LDBFactory extends ScorexLogging {

  def openDb(path: File, options: Options = new Options()): RocksDB = {
    path.mkdirs()
    options.setCreateIfMissing(true)
      .setWriteBufferSize(128 * SizeUnit.MB)
      .setMaxWriteBufferNumber(3)
      .setMaxBackgroundJobs(10)
      .setCompressionType(CompressionType.NO_COMPRESSION)
      .setCompactionStyle(CompactionStyle.LEVEL)
    RocksDB.open(options, path.toString)
  }

  def createKvDb(path: File): LDBKVStore = {
    try {
      new LDBKVStore(openDb(path))
    } catch {
      case x: Throwable =>
        log.error(s"Failed to initialize storage: $x. Please check that directory $path could be accessed " +
          s"and is not used by some other active node")
        java.lang.System.exit(2)
        null
    }
  }

}
