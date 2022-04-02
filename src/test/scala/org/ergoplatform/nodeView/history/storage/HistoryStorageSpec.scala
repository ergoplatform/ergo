package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.HistoryTestHelpers
import org.scalacheck.Gen
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, idToBytes}

class HistoryStorageSpec extends HistoryTestHelpers {

  val db = HistoryStorage(settings)

  property("Write Read Remove") {
    val headers = Gen.listOfN(20, defaultHeaderGen).sample.get
    val modifiers = Gen.listOfN(20, randomADProofsGen).sample.get
    def validityKey(id: ModifierId) = ByteArrayWrapper(Algos.hash("validity".getBytes(ErgoHistory.CharsetName) ++ idToBytes(id)))
    val indexes = headers.flatMap(h => Seq(validityKey(h.id) -> Array(1.toByte)))
    db.insert(indexes, headers ++ modifiers) shouldBe 'success

    headers.forall(h => db.contains(h.id)) shouldBe true
    modifiers.forall(m => db.contains(m.id)) shouldBe true

    headers.forall(h => db.get(h.id).exists(_.nonEmpty)) shouldBe true
    modifiers.forall(m => db.get(m.id).exists(_.nonEmpty)) shouldBe true
    indexes.forall(i => db.getIndex(i._1).exists(_.nonEmpty)) shouldBe true

    db.remove(indexes.map(_._1), headers.map(_.id) ++ modifiers.map(_.id))

    headers.forall(h => !db.contains(h.id)) shouldBe true
    modifiers.forall(m => !db.contains(m.id)) shouldBe true

    headers.forall(h => !db.get(h.id).exists(_.nonEmpty)) shouldBe true
    modifiers.forall(m => !db.get(m.id).exists(_.nonEmpty)) shouldBe true
    indexes.forall(i => !db.getIndex(i._1).exists(_.nonEmpty)) shouldBe true
  }

}
