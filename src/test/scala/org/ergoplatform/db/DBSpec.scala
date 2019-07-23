package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.Algos
import org.rocksdb.{Options, RocksDB}
import scorex.testkit.utils.FileUtils

trait DBSpec extends FileUtils {

  RocksDB.loadLibrary()

  implicit class ValueOps(x: Option[Array[Byte]]) {
    def toBs: Option[ByteString] = x.map(ByteString.apply)
  }

  implicit class KeyValueOps(xs: Seq[(Array[Byte], Array[Byte])]) {
    def toBs: Seq[(ByteString, ByteString)] = xs.map(x => ByteString(x._1) -> ByteString(x._2))
  }

  protected def byteString(s: String): Array[Byte] = s.getBytes("UTF-8")

  protected def byteString32(s: String): Array[Byte] = Algos.hash(s.getBytes("UTF-8"))

  protected def withDb(body: RocksDB => Unit): Unit = {
    val options = new Options().setCreateIfMissing(true)
    val db = RocksDB.open(options, createTempDir.getAbsolutePath)
    try body(db) finally db.close()
  }

  protected def versionId(s: String): Array[Byte] = Algos.hash(s.getBytes("UTF-8"))

  protected def withStore(body: LDBKVStore => Unit): Unit =
    withDb { db: RocksDB => body(new LDBKVStore(db)) }

  protected def withVersionedStore(keepVersions: Int)(body: VersionedLDBKVStore => Unit): Unit =
    withDb { db: RocksDB => body(new VersionedLDBKVStore(db, keepVersions)) }

}
