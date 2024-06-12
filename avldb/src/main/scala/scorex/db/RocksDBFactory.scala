package scorex.db

import org.rocksdb._
import org.rocksdb.util.SizeUnit
import scorex.util.ScorexLogging

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.collection.mutable

/**
  * Registry of opened RocksDB instances.
  * RocksDB prohibit access to the same storage file from more than one DB instance.
  * And ergo application (mostly tests) quite frequently doesn't not explicitly close
  * database and tries to reopen it.
  */
object RocksDBFactory extends ScorexLogging {

  RocksDB.loadLibrary()

  private val lock = new ReentrantReadWriteLock()
  private val map = new mutable.HashMap[File, RegisteredDB]

  /**
   * Decorator of RocksDB class which overrides close() methods and unlinks database from registry on close.
   * So if database was not explicitly closed, then next attempt to open database with the same path will
   * return existed instance instead of creating new one.
   */
  case class RegisteredDB(impl: RocksDB, path: File) {
    val open: AtomicBoolean = new AtomicBoolean(true)
    def get(key: Array[Byte]): Array[Byte] = {
      if(open.get())
        impl.get(key)
      else
        null
    }
    def get(options: ReadOptions, key: Array[Byte]): Array[Byte] = impl.get(options, key)
    def contains(key: Array[Byte]): Boolean = impl.keyExists(key)
    def iterator: RocksIterator = impl.newIterator()
    def iterator(options: ReadOptions): RocksIterator = impl.newIterator(options)
    def put(key: Array[Byte], value: Array[Byte]): Unit = impl.put(key, value)
    def write(options: WriteOptions, batch: WriteBatch): Unit = impl.write(options, batch)
    def getSnapshot: Snapshot = impl.getSnapshot
    def releaseSnapshot(snapshot: Snapshot): Unit = impl.releaseSnapshot(snapshot)
    def close(): Unit = {
      lock.writeLock().lock()
      try {
        map.remove(path)
        impl.close()
        open.set(false)
      } finally {
        lock.writeLock().unlock()
      }
    }
  }

  private val normalOptions: Options =  new Options()
    .setCreateIfMissing(true)
    .setWriteBufferSize(32 * SizeUnit.MB)
    .setAllowMmapReads(true)
    .setIncreaseParallelism(4)
    .setCompressionType(CompressionType.LZ4_COMPRESSION)
    .setCompactionStyle(CompactionStyle.LEVEL)

  private val testOptions: Options =  new Options()
    .setCreateIfMissing(true)
    .setWriteBufferSize(64 * SizeUnit.KB)
    .setManifestPreallocationSize(32 * SizeUnit.KB)
    .setCompressionType(CompressionType.LZ4_COMPRESSION)
    .setCompactionStyle(CompactionStyle.LEVEL)

  def open(path: File): RegisteredDB = {
    lock.writeLock().lock()
    try {
      path.mkdirs()
      val options = if(System.getProperty("env") == "test") testOptions else normalOptions
      map.getOrElseUpdate(path, RegisteredDB(RocksDB.open(options, path.toString), path))
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
