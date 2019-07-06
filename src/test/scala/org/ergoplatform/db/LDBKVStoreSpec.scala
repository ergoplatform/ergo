package org.ergoplatform.db

import org.iq80.leveldb.impl.Iq80DBFactory._

class LDBKVStoreSpec extends DBSpec {

  property("LDB value rewrite") {
    withDb { db =>
      val key = bytes("x")
      val batch = db.createWriteBatch()
      batch.put(key, bytes("1"))
      batch.delete(key)
      batch.put(key, bytes("2"))
      db.write(batch)

      asString(db.get(key)) shouldBe "2"
    }
  }

}
