package org.ergoplatform.db

import org.scalatest.{Matchers, PropSpec}

class VersionedStoreSpec extends PropSpec with Matchers with DBSpec {

  private val (keyA, valA) = (byteString("A"), byteString("1"))
  private val (keyB, valB) = (byteString("B"), byteString("2"))
  private val (keyC, valC) = (byteString("C"), byteString("3"))
  private val (keyD, valD) = (byteString("D"), byteString("4"))
  private val (keyE, valE) = (byteString("E"), byteString("5"))

  private val v1 = versionId("v1")
  private val v2 = versionId("v2")
  private val v3 = versionId("v3")
  private val v4 = versionId("v4")

  property("rollback (1 version back)") {
    withVersionedStore(10) { store =>
      store.insert(v1, Seq(keyA -> valA))
      store.insert(v2, Seq(keyB -> valB, keyC -> valC))
      store.update(v3, Seq(keyC), Seq(keyA -> byteString("6"), keyD -> valD))

      store.getAll.toSeq.toBs should contain allElementsOf Seq(
        keyA -> byteString("6"),
        keyB -> valB,
        keyD -> valD
      ).toBs

      store.rollback(v2) shouldBe 'success

      store.getAll.toSeq.toBs should contain allElementsOf Seq(keyA -> valA, keyB -> valB, keyC -> valC).toBs
      store.get(keyD) shouldBe None
    }
  }

  property("rollback (2 versions back)") {
    withVersionedStore(10) { store =>
      store.insert(v1, Seq(keyA -> valA))
      store.insert(v2, Seq(keyB -> valB, keyC -> valC))
      store.update(v3, Seq(keyC), Seq(keyA -> byteString("6"), keyD -> valD))
      store.update(v4, Seq(keyA), Seq(keyB -> byteString("7"), keyE -> valE))

      store.getAll.toSeq.toBs should contain allElementsOf Seq(keyB -> byteString("7"), keyE -> valE, keyD -> valD).toBs
      store.get(keyC) shouldBe None
      store.get(keyA) shouldBe None

      store.rollback(v2) shouldBe 'success

      store.getAll.toSeq.toBs should contain allElementsOf Seq(keyA -> valA, keyB -> valB, keyC -> valC).toBs
      store.get(keyE) shouldBe None
      store.get(keyD) shouldBe None
    }
  }

  property("Outdated versions pruning") {
    withVersionedStore(2) { store =>
      store.insert(v1, Seq(keyA -> valA))
      store.insert(v2, Seq(keyB -> valB))

      store.versionIdExists(v1) shouldBe true

      store.insert(v3, Seq(keyC -> valC))

      store.versionIdExists(v3) shouldBe true
      store.versionIdExists(v2) shouldBe true
      store.versionIdExists(v1) shouldBe false

      store.get(keyA).isDefined shouldBe true
      store.get(keyB).isDefined shouldBe true
      store.get(keyC).isDefined shouldBe true

      store.rollback(v1).isSuccess shouldBe false

      store.get(keyA).isDefined shouldBe true
      store.get(keyB).isDefined shouldBe true
      store.get(keyC).isDefined shouldBe true
    }
  }

}
