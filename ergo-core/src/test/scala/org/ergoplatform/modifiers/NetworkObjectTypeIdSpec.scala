package org.ergoplatform.modifiers

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class NetworkObjectTypeIdSpec extends AnyPropSpec with ScalaCheckPropertyChecks {

  // Known type IDs from the implementation
  val knownTypeIds: Set[Byte] = Set(
    101, 102, 104, 108, // Block section types
    2, -127, -126, -125, -124, -123, -122, -121 // Auxiliary types
  ).map(_.toByte)

  property("isTypeKnown should return true for all known type IDs") {
    forAll(Gen.oneOf(knownTypeIds.toSeq)) { byteValue =>
      val typeId = NetworkObjectTypeId.fromByte(byteValue)
      assert(NetworkObjectTypeId.isTypeKnown(typeId))
    }
  }

  property("isTypeKnown should return false for unknown type IDs") {
    // Generate bytes that are not in the known type IDs
    val unknownByteGen = Gen
      .choose(Byte.MinValue, Byte.MaxValue)
      .suchThat(b => !knownTypeIds.contains(b))

    forAll(unknownByteGen) { byteValue =>
      val typeId = NetworkObjectTypeId.fromByte(byteValue)
      assert(!NetworkObjectTypeId.isTypeKnown(typeId))
    }
  }

  property("isBlockSection should correctly identify block sections") {
    forAll(Gen.oneOf(knownTypeIds.toSeq)) { byteValue =>
      val typeId         = NetworkObjectTypeId.fromByte(byteValue)
      val isBlockSection = NetworkObjectTypeId.isBlockSection(typeId)

      // If it's a known type and a block section, it should be ≥50
      if (isBlockSection) {
        assert(byteValue >= 50)
      } else {
        assert(byteValue < 50)
      }
    }
  }

}
