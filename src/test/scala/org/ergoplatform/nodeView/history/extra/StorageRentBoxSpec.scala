package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.bytesToId
import scorex.util.encode.Base16

/**
  * Unit pins for the storage-rent eligibility index entry: key-based reconstruction (rows
  * carry no payload), and the lexicographic key ordering by (creationHeight, globalIndex)
  * that the range scan relies on.
  */
class StorageRentBoxSpec extends ErgoCorePropertyTest {

  private def entry(creationHeight: Int, globalIndex: Long): StorageRentBox =
    new StorageRentBox(creationHeight, globalIndex)

  property("row value is just the type byte, entry is reconstructed from the key") {
    val srb = entry(123456, 789012345L)
    // the serialized row is the type byte only - no payload
    ExtraIndexSerializer.toBytes(srb).toSeq shouldBe Seq(StorageRentBox.extraIndexTypeId)
    // the scan side reconstructs the entry from the key
    val parsed = StorageRentBox.fromKey(srb.serializedId)
    parsed.creationHeight shouldBe srb.creationHeight
    parsed.globalIndex shouldBe srb.globalIndex
    parsed.serializedId shouldBe srb.serializedId
    parsed.id shouldBe srb.id
  }

  property("value-based parsing of a rent row degrades to Failure, not a thrown Error") {
    // the row value alone can not reconstruct the entry, but a misbehaving reader (or a
    // corrupted store) must hit the same NonFatal failure path as any other parse error,
    // so Try-based callers (getExtraIndex) get None with a log instead of a crash
    val srb = entry(123456, 789012345L)
    val res = ExtraIndexSerializer.parseBytesTry(ExtraIndexSerializer.toBytes(srb))
    res.isFailure shouldBe true
  }

  property("an unknown type byte degrades to Failure, not a thrown Error") {
    // 99 is not assigned to any extra index type; parsing it must fail non-fatally
    ExtraIndexSerializer.parseBytesTry(Array(99.toByte, 1, 2, 3)).isFailure shouldBe true
  }

  property("keys order lexicographically by (creationHeight, globalIndex)") {
    def hex(key: Array[Byte]): String = Base16.encode(key)

    val cases = Seq(
      (entry(100, 5L), entry(100, 6L)),
      (entry(100, 6L), entry(101, 0L)),
      (entry(0, 0L), entry(1, 0L)),
      (entry(Int.MaxValue, 0L), entry(Int.MaxValue, 1L))
    )
    cases.foreach { case (a, b) =>
      withClue(s"${hex(a.serializedId)} < ${hex(b.serializedId)}: ") {
        scorex.db.ByteArrayUtils.compare(a.serializedId, b.serializedId) should be < 0
      }
    }
  }

  property("keys share the marker prefix and length") {
    val srb = entry(42, 7L)
    srb.serializedId.length shouldBe StorageRentBox.KeyLength
    srb.serializedId.head shouldBe StorageRentBox.KeyMarker
    // id is stable and derived from the key
    srb.id shouldBe bytesToId(srb.serializedId)
  }

  property("entry is derived from an IndexedErgoBox") {
    val box = sigmastate.helpers.TestingHelpers.testBox(1000000000L,
      org.ergoplatform.settings.Constants.TrueTree, 777)
    val iEb = new IndexedErgoBox(777, None, None, None, box, 424242L)
    val srb = StorageRentBox(iEb)
    srb.creationHeight shouldBe box.creationHeight
    srb.globalIndex shouldBe iEb.globalIndex
  }

}
