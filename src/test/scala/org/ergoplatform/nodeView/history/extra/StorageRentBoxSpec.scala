package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.bytesToId
import scorex.util.encode.Base16

/**
  * Unit pins for the storage-rent eligibility index entry: serialization round-trip through
  * [[ExtraIndexSerializer]], and the lexicographic key ordering by (creationHeight,
  * globalIndex) that the range scan relies on.
  */
class StorageRentBoxSpec extends ErgoCorePropertyTest {

  private def entry(creationHeight: Int, globalIndex: Long): StorageRentBox =
    new StorageRentBox(creationHeight, globalIndex,
      bytesToId(Array.fill(32)(1.toByte)), 1000000000L, 76)

  property("serialization roundtrip via ExtraIndexSerializer") {
    val srb = entry(123456, 789012345L)
    val bytes = ExtraIndexSerializer.toBytes(srb)
    val parsed = ExtraIndexSerializer.parseBytes(bytes)
    parsed.isInstanceOf[StorageRentBox] shouldBe true
    val parsedSrb = parsed.asInstanceOf[StorageRentBox]
    parsedSrb.creationHeight shouldBe srb.creationHeight
    parsedSrb.globalIndex shouldBe srb.globalIndex
    parsedSrb.boxId shouldBe srb.boxId
    parsedSrb.value shouldBe srb.value
    parsedSrb.bytesLen shouldBe srb.bytesLen
    parsedSrb.serializedId shouldBe srb.serializedId
    parsedSrb.id shouldBe srb.id
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
    srb.boxId shouldBe iEb.id
    srb.value shouldBe box.value
    srb.bytesLen shouldBe box.bytes.length
  }

}
