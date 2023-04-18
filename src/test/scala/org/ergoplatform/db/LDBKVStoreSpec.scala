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

      store.insert(Array(key -> valA)).get

      store.get(key).toBs shouldBe Some(valA).toBs

      store.insert(Array(key -> valB)).get

      store.get(key).toBs shouldBe Some(valB).toBs

      store.getAll.size shouldBe 1
    }
  }

  property("last key in range") {
    withStore { store =>
      val valueA = (byteString("A"), byteString("1"))
      val valueB = (byteString("B"), byteString("2"))
      val valueC = (byteString("C"), byteString("1"))
      val valueD = (byteString("D"), byteString("2"))
      val valueE = (byteString("E"), byteString("3"))
      val valueF = (byteString("F"), byteString("4"))

      store.insert(Array(valueA, valueB, valueC, valueD, valueE, valueF)).get

      store.lastKeyInRange(valueA._1, valueC._1).get.toSeq shouldBe valueC._1.toSeq
      store.lastKeyInRange(valueD._1, valueF._1).get.toSeq shouldBe valueF._1.toSeq
      store.lastKeyInRange(valueF._1, byteString32("Z")).get.toSeq shouldBe valueF._1.toSeq
      store.lastKeyInRange(Array(10: Byte), valueA._1).get.toSeq shouldBe valueA._1.toSeq

      store.lastKeyInRange(Array(10: Byte), Array(11: Byte)).isDefined shouldBe false
    }
  }

}
