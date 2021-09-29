package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.Algos
import org.iq80.leveldb.{DB, Options}
import scorex.testkit.utils.FileUtils
import scorex.db.{SWDBFactory, SWDBStore, SWDBVersionedStore}

trait DBSpec extends FileUtils {

  implicit class ValueOps(x: Option[Array[Byte]]) {
    def toBs: Option[ByteString] = x.map(ByteString.apply)
  }

  implicit class KeyValueOps(xs: Seq[(Array[Byte], Array[Byte])]) {
    def toBs: Seq[(ByteString, ByteString)] = xs.map(x => ByteString(x._1) -> ByteString(x._2))
  }

  protected def byteString(s: String): Array[Byte] = s.getBytes("UTF-8")

  protected def byteString32(s: String): Array[Byte] = Algos.hash(byteString(s))

  protected def versionId(s: String): Array[Byte] = byteString32(s)

  protected def withStore(body: SWDBStore => Unit): Unit = {
    val db = SWDBFactory.create(createTempDir)
    try body(db) finally db.close()
  }

  protected def withVersionedStore(keepVersions: Int)(body: SWDBVersionedStore => Unit): Unit = {
    val db = new SWDBVersionedStore(createTempDir, keepVersions)
    try body(db) finally db.close()
  }

}
