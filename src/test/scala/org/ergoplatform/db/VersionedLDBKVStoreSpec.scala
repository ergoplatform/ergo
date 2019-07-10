package org.ergoplatform.db

import org.ergoplatform.settings.Constants
import org.scalatest.{Matchers, PropSpec}

class VersionedLDBKVStoreSpec extends PropSpec with Matchers with DBSpec {

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
      store.insert(Seq(keyA -> valA))(v1)
      store.insert(Seq(keyB -> valB, keyC -> valC))(v2)
      store.update(toInsert = Seq(keyA -> byteString("6"), keyD -> valD), toRemove = Seq(keyC))(v3)

      store.getAll should contain allElementsOf Seq(
        keyA -> byteString("6"),
        keyB -> valB,
        keyD -> valD
      )

      store.rollbackTo(v2) shouldBe 'success

      store.getAll should contain allElementsOf Seq(keyA -> valA, keyB -> valB, keyC -> valC)
      store.get(keyD) shouldBe None
    }
  }

  property("rollback (2 versions back)") {
    withVersionedStore(10) { store =>
      store.insert(Seq(keyA -> valA))(v1)
      store.insert(Seq(keyB -> valB, keyC -> valC))(v2)
      store.update(toInsert = Seq(keyA -> byteString("6"), keyD -> valD), toRemove = Seq(keyC))(v3)
      store.update(toInsert = Seq(keyB -> byteString("7"), keyE -> valE), toRemove = Seq(keyA))(v4)

      store.getAll should contain allElementsOf Seq(keyB -> byteString("7"), keyE -> valE, keyD -> valD)
      store.get(keyC) shouldBe None
      store.get(keyA) shouldBe None

      store.rollbackTo(v2) shouldBe 'success

      store.getAll should contain allElementsOf Seq(keyA -> valA, keyB -> valB, keyC -> valC)
      store.get(keyE) shouldBe None
      store.get(keyD) shouldBe None
    }
  }

  property("Outdated versions pruning") {
    withVersionedStore(2) { store =>
      store.insert(Seq(keyA -> valA))(v1)
      store.insert(Seq(keyB -> valB))(v2)

      store.get(v1).isDefined shouldBe true

      store.insert(Seq(keyC -> valC))(v3)

      store.get(v3).isDefined shouldBe true
      store.get(v2).isDefined shouldBe true
      store.get(v1) shouldBe None

      store.get(store.VersionsKey).toSeq
        .flatMap(_.grouped(Constants.HashLength)) should contain theSameElementsAs Seq(v2, v3)
    }
  }

}
