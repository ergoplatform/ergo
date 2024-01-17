package org.ergoplatform.db

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class LDBKVStoreSpec extends AnyPropSpec with Matchers with DBSpec {

  property("put/get/getAll/delete") {
    withStore { store =>
      val valueA = (byteString("A"), byteString("1"))
      val valueB = (byteString("B"), byteString("2"))

      store.update(Array(valueA._1, valueB._1), Array(valueA._2, valueB._2), toRemove = Array.empty).get

      store.get(valueA._1).toBs shouldBe Some(valueA._2).toBs
      store.get(valueB._1).toBs shouldBe Some(valueB._2).toBs

      store.getAll.toSeq.toBs shouldBe Seq(valueA, valueB).toBs

      store.update(Array.empty, Array.empty, toRemove = Array(valueA._1)).get
      store.get(valueA._1) shouldBe None
    }
  }

  property("record rewriting") {
    withStore { store =>
      val key = byteString("A")
      val valA = byteString("1")
      val valB = byteString("2")

      store.insert(key, valA).get

      store.get(key).toBs shouldBe Some(valA).toBs

      store.insert(key, valB).get

      store.get(key).toBs shouldBe Some(valB).toBs

      store.getAll.size shouldBe 1
    }
  }

}
