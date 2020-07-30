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

      // keys before the range
      store.getRange(byteString("a"), byteString("z")).length shouldBe 0

      // keys inside the range
      store.getRange(byteString("<"), byteString("z")).length shouldBe 2

      // keys after the range
      store.getRange(byteString("<"), byteString("?")).length shouldBe 0

      //removing keys
      store.remove(Seq(keyStart, keyEnd))
      store.getRange(keyStart, keyEnd).length shouldBe 0
    }
  }

}
