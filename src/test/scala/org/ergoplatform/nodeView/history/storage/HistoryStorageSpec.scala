package org.ergoplatform.nodeView.history.storage

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, IndexedErgoBox, StorageRentBox}
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.db.ByteArrayWrapper
import scorex.util.{ModifierId, bytesToId, idToBytes}
import sigmastate.helpers.TestingHelpers.testBox

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

  /** Insert a distinct box and its storage-rent eligibility entry, return the box row. */
  private def insertRentBox(globalIndex: Long): IndexedErgoBox = {
    val creationHeight = 1000 + globalIndex.toInt
    val box = testBox(1000000000L, Constants.TrueTree, creationHeight)
    val iEb = new IndexedErgoBox(creationHeight, None, None, None, box, globalIndex)
    db.insertExtra(Array.empty, Array[ExtraIndex](iEb, StorageRentBox(iEb)))
    iEb
  }

  private def rentBoxIdsInIndex(limit: Int): Seq[ModifierId] =
    db.storageRentBoxesUntil(Int.MaxValue, limit).map(_.boxId).toSeq

  property("storage rent entries are removable by box id") {
    val iEbs = (0L until 3).map(insertRentBox)
    val boxIds = iEbs.map(_.id)
    rentBoxIdsInIndex(10).toSet shouldBe boxIds.toSet

    // removing a subset removes exactly those entries, in key order for the rest
    db.removeStorageRentBoxes(boxIds.take(2))
    rentBoxIdsInIndex(10) shouldBe Seq(boxIds(2))

    // repeated removal is a no-op; unknown box ids are ignored
    db.removeStorageRentBoxes(boxIds.take(2) :+ bytesToId(Array.fill(32)(42.toByte)))
    rentBoxIdsInIndex(10) shouldBe Seq(boxIds(2))

    // an entry whose IndexedErgoBox is gone can not be located, so it is left in place
    db.removeExtra(Array(boxIds(2)))
    db.removeStorageRentBoxes(Seq(boxIds(2)))
    rentBoxIdsInIndex(10) shouldBe Seq(boxIds(2))
  }

}
