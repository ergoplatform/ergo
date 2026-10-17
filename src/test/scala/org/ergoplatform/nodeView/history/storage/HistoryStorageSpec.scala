package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.db.{ByteArrayWrapper, LDBKVStore}
import scorex.util.{ModifierId, idToBytes}

import scala.util.{Failure, Success, Try}

class HistoryStorageSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  val db = HistoryStorage(settings)

  property("Write Read Remove") {
    val headers: Array[Header] = Gen.listOfN(20, defaultHeaderGen).sample.get.toArray
    val modifiers: Array[ADProofs] = Gen.listOfN(20, randomADProofsGen).sample.get.toArray
    def validityKey(id: ModifierId) = ByteArrayWrapper(Algos.hash("validity".getBytes(CharsetName) ++ idToBytes(id)))
    val indexes = headers.flatMap(h => Array(validityKey(h.id) -> Array(1.toByte)))
    db.insert(indexes, (headers ++ modifiers).asInstanceOf[Array[BlockSection]]) shouldBe 'success

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

  property("remove propagates an index store failure after the object store succeeds") {
    val sentinel = new IllegalStateException("index removal sentinel")
    val objectsStore = new LDBKVStore(null) {
      override def update(toInsertKeys: Array[Array[Byte]], toInsertValues: Array[Array[Byte]],
                          toRemove: Array[Array[Byte]]): Try[Unit] = Success(())
    }
    val indexStore = new LDBKVStore(null) {
      override def update(toInsertKeys: Array[Array[Byte]], toInsertValues: Array[Array[Byte]],
                          toRemove: Array[Array[Byte]]): Try[Unit] = Failure(sentinel)
    }
    val storage = new HistoryStorage(indexStore, objectsStore, null, settings.cacheSettings)
    val key = ByteArrayWrapper(Array.fill[Byte](32)(1))

    storage.remove(Array(key), Array.empty[ModifierId]) shouldBe Failure(sentinel)
  }

}
