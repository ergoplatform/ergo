package org.ergoplatform.db

import akka.util.ByteString
import org.ergoplatform.settings.Algos
import org.iq80.leveldb.impl.Iq80DBFactory.bytes
import org.iq80.leveldb.{DB, Options}
import scorex.testkit.utils.FileUtils

trait DBSpec extends FileUtils {

  import LDBFactory.factory

  protected def byteString(s: String) = ByteString(bytes(s))

  protected def withDb(body: DB => Unit): Unit = {
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(createTempDir, options)
    try body(db) finally db.close()
  }

  protected def versionId(s: String) = ByteString(Algos.hash(bytes(s)))

  protected def withStore(body: LDBKVStore => Unit): Unit =
    withDb { db: DB => body(new LDBKVStore(db)) }

  protected def withVersionedStore(body: VersionedLDBKVStore => Unit): Unit =
    withDb { db: DB => body(new VersionedLDBKVStore(db, 100)) }

}
