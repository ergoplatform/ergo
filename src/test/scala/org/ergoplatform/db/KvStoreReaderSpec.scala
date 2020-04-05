package org.ergoplatform.db

import org.scalatest.{Matchers, PropSpec}

class KvStoreReaderSpec extends PropSpec with Matchers with DBSpec {

  property("getRange works properly") {
    withStore {store =>
      val keyStart = byteString("A")
      val keyEnd = byteString("Z")
      store.getRange(keyStart, keyEnd).length shouldBe 0

      store.insert(Seq(keyStart -> keyStart))
      store.getRange(keyStart, keyEnd).length shouldBe 1

      store.insert(Seq(keyEnd -> keyEnd))
      store.getRange(keyStart, keyEnd).length shouldBe 2

      store.remove(Seq(keyStart, keyEnd))
      store.getRange(keyStart, keyEnd).length shouldBe 0
    }
  }

}
