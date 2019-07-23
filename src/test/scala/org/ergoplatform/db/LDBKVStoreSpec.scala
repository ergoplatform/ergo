package org.ergoplatform.db

import org.rocksdb.RocksDB
import org.scalatest.{Matchers, PropSpec}

class LDBKVStoreSpec extends PropSpec with Matchers with DBSpec {

  property("put/get/getAll/delete") {
    withStore { store =>
      val valueA = (byteString("A"), byteString("1"))
      val valueB = (byteString("B"), byteString("2"))

      store.update(toInsert = Seq(valueA, valueB), toRemove = Seq.empty)

      store.get(valueA._1).toBs shouldBe Some(valueA._2).toBs
      store.get(valueB._1).toBs shouldBe Some(valueB._2).toBs

      store.getAll.toBs shouldBe Seq(valueA, valueB).toBs

      store.update(toInsert = Seq.empty, toRemove = Seq(valueA._1))
      store.get(valueA._1) shouldBe None
    }
  }

  property("record rewriting") {
    withStore { store =>
      val key = byteString("A")
      val valA = byteString("1")
      val valB = byteString("2")

      store.insert(Seq(key -> valA))

      store.get(key).toBs shouldBe Some(valA).toBs

      store.insert(Seq(key -> valB))

      store.get(key).toBs shouldBe Some(valB).toBs

      store.getAll.size shouldBe 1
    }
  }

}
