package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.Algos
import org.ergoplatform.wallet.utils.TestFileUtils
import org.iq80.leveldb.{DB, Options}
import scorex.db.LDBFactory.factory
import scorex.db.{LDBKVStore, LDBVersionedStore}

trait DBSpec extends TestFileUtils {

  implicit class ValueOps(x: Option[Array[Byte]]) {
    def toBs: Option[ByteString] = x.map(ByteString.apply)
  }

  implicit class KeyValueOps(xs: Seq[(Array[Byte], Array[Byte])]) {
    def toBs: Seq[(ByteString, ByteString)] = xs.map(x => ByteString(x._1) -> ByteString(x._2))
  }

  protected def byteString(s: String): Array[Byte] = s.getBytes("UTF-8")

  protected def byteString32(s: String): Array[Byte] = Algos.hash(byteString(s))

  protected def withDb[T](body: DB => T): T = {
    val options = new Options()
    options.createIfMissing(true)
    options.verifyChecksums(true)
    options.maxOpenFiles(2000)
    val db = factory.open(createTempDir, options)
    try body(db) finally db.close()
  }

  protected def versionId(s: String): Array[Byte] = byteString32(s)

  protected def withStore[T](body: LDBKVStore => T): T =
    withDb { db: DB => body(new LDBKVStore(db)) }

  protected def withVersionedStore[T](keepVersions: Int)(body: LDBVersionedStore => T): T = {
    val db = new LDBVersionedStore(createTempDir, keepVersions)
    try body(db) finally db.close()
  }

}
