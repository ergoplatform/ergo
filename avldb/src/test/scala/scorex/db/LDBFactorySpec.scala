package scorex.db

import org.iq80.leveldb.{DB, DBFactory, DBIterator, Options, Range, ReadOptions, Snapshot, WriteBatch, WriteOptions}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.authds.avltree.batch.benchmark.LDBVersionedStoreBenchmark.getRandomTempDir

import java.io.File

class LDBFactorySpec extends AnyPropSpec with Matchers {

  private def mockDb(): DB = new DB {
    override def get(key: Array[Byte]): Array[Byte] = null
    override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = null
    override def iterator: DBIterator = ???
    override def iterator(options: ReadOptions): DBIterator = ???
    override def put(key: Array[Byte], value: Array[Byte]): Unit = ()
    override def delete(key: Array[Byte]): Unit = ()
    override def write(batch: WriteBatch): Unit = ()
    override def write(batch: WriteBatch, options: WriteOptions): Snapshot = ???
    override def createWriteBatch: WriteBatch = ???
    override def put(key: Array[Byte], value: Array[Byte], options: WriteOptions): Snapshot = ???
    override def delete(key: Array[Byte], options: WriteOptions): Snapshot = ???
    override def getSnapshot: Snapshot = ???
    override def getApproximateSizes(ranges: Range*): Array[Long] = ???
    override def getProperty(name: String): String = ???
    override def suspendCompactions(): Unit = ()
    override def resumeCompactions(): Unit = ()
    override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit = ()
    override def close(): Unit = ()
  }

  property("StoreRegistry should repair and retry on open failure") {
    val dir = getRandomTempDir
    val db = mockDb()

    var openCalls = 0
    var repairCalls = 0

    val mockFactory = new DBFactory {
      override def open(path: File, options: Options): DB = {
        openCalls += 1
        if (openCalls == 1) {
          throw new RuntimeException("Simulated corruption: Structure needs cleaning")
        }
        db
      }

      override def destroy(path: File, options: Options): Unit = ()

      override def repair(path: File, options: Options): Unit = {
        repairCalls += 1
      }
    }

    val registry = StoreRegistry(mockFactory)
    val result = registry.open(dir, new Options())

    result should not be null
    openCalls shouldBe 2
    repairCalls shouldBe 1
  }

  property("StoreRegistry should pass through on successful open") {
    val dir = getRandomTempDir
    val db = mockDb()

    var openCalls = 0
    var repairCalls = 0

    val mockFactory = new DBFactory {
      override def open(path: File, options: Options): DB = {
        openCalls += 1
        db
      }

      override def destroy(path: File, options: Options): Unit = ()

      override def repair(path: File, options: Options): Unit = {
        repairCalls += 1
      }
    }

    val registry = StoreRegistry(mockFactory)
    val result = registry.open(dir, new Options())

    result should not be null
    openCalls shouldBe 1
    repairCalls shouldBe 0
  }

  property("LDBFactory.createKvDb should create and open database") {
    val dir = getRandomTempDir
    val store = LDBFactory.createKvDb(dir.getAbsolutePath)
    store should not be null
    // verify the store is functional by inserting and reading a key
    val key = Array[Byte](1, 2, 3)
    val value = Array[Byte](4, 5, 6)
    store.insert(key, value).isSuccess shouldBe true
    store.get(key).exists(_.sameElements(value)) shouldBe true
  }

}
