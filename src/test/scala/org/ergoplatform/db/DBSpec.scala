package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.Algos
import org.iq80.leveldb.impl.Iq80DBFactory.bytes
import org.iq80.leveldb.{DB, Options}
import scorex.crypto.hash.Digest32
import scorex.testkit.utils.FileUtils

trait DBSpec extends FileUtils {

  import LDBFactory.factory

  implicit class ValueOps(x: Option[Array[Byte]]) {
    def toBs: Option[ByteString] = x.map(ByteString.apply)
  }

  implicit class KeyValueOps(xs: Seq[(Array[Byte], Array[Byte])]) {
    def toBs: Seq[(ByteString, ByteString)] = xs.map(x => ByteString(x._1) -> ByteString(x._2))
  }

  protected def byteString(s: String): Array[Byte] = bytes(s)

  protected def withDb(body: DB => Unit): Unit = {
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(createTempDir, options)
    try body(db) finally db.close()
  }

  protected def versionId(s: String): Digest32 = Algos.hash(bytes(s))

  protected def withStore(body: LDBKVStore => Unit): Unit =
    withDb { db: DB => body(new LDBKVStore(db)) }

  protected def withVersionedStore(keepVersions: Int)(body: VersionedLDBKVStore => Unit): Unit =
    withDb { db: DB => body(new VersionedLDBKVStore(db, keepVersions)) }

}
