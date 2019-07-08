package org.ergoplatform.db

import org.scalatest.{Matchers, PropSpec}

class LDBKVStoreSpec extends PropSpec with Matchers with DBSpec {

  property("put/get/getAll/delete") {
    withStore { store =>
      val valueA = (byteString("A"), byteString("1"))
      val valueB = (byteString("B"), byteString("2"))

      store.update(toInsert = Seq(valueA, valueB), toRemove = Seq.empty)

      store.get(valueA._1) shouldBe Some(valueA._2)
      store.get(valueB._1) shouldBe Some(valueB._2)

      store.getAll shouldBe Seq(valueA, valueB)

      store.update(toInsert = Seq.empty, toRemove = Seq(valueA._1))
      store.get(valueA._1) shouldBe None
    }
  }

}
