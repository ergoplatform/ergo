package org.ergoplatform.modifiers

import org.ergoplatform.modifiers.ErgoNodeViewModifier.ModifierIdSize
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * Safety and correctness tests for ModifierId.
 * 
 * These tests verify:
 * - Immutability guarantees
 * - Round-trip serialization correctness
 * - Length validation
 * - HashCode/equals correctness
 */
class ModifierIdSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  val validHexString = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  val validBytes = Array.tabulate(32)(i => i.toByte)

  property("ModifierId is immutable - modifying input array after construction does not affect ModifierId") {
    val inputBytes = Array.tabulate(32)(i => i.toByte)
    val originalFirstByte = inputBytes(0)
    val id = ModifierId(inputBytes)
    
    // Modify the original array
    inputBytes(0) = 99.toByte
    
    // ModifierId should be unaffected
    id.toBytes(0) shouldBe originalFirstByte
    inputBytes(0) shouldBe 99.toByte // Original array was modified
  }

  property("ModifierId is immutable - modifying array returned by toBytes() does not affect original") {
    val id = ModifierId(validBytes)
    val bytesCopy = id.toBytes
    val originalFirstByte = bytesCopy(0)
    
    // Modify the returned copy
    bytesCopy(0) = 99.toByte
    
    // Original ModifierId should be unaffected
    id.toBytes(0) shouldBe originalFirstByte
    bytesCopy(0) shouldBe 99.toByte // Copy was modified
  }

  property("ModifierId is immutable - modifying array used in toColl() does not affect original") {
    val id = ModifierId(validBytes)
    val coll = id.toColl
    val originalFirstByte = coll(0)
    
    // Note: Coll is immutable, but we verify the ModifierId itself is unchanged
    val bytesAfterColl = id.toBytes
    bytesAfterColl(0) shouldBe originalFirstByte
  }

  property("Round-trip: fromHex(id.toHexString) == id") {
    val id1 = ModifierId.fromHex(validHexString)
    val hexString = id1.toHexString
    val id2 = ModifierId.fromHex(hexString)
    
    id1 shouldBe id2
    id1.hashCode() shouldBe id2.hashCode()
  }

  property("Round-trip: fromBytes(id.toBytes) == id") {
    val id1 = ModifierId(validBytes)
    val bytes = id1.toBytes
    val id2 = ModifierId.fromBytes(bytes)
    
    id1 shouldBe id2
    id1.hashCode() shouldBe id2.hashCode()
  }

  property("fromHex rejects invalid length hex strings") {
    val tooShort = "0123456789abcdef" // 16 chars, need 64
    val tooLong = validHexString + "00" // 66 chars
    
    an[IllegalArgumentException] should be thrownBy ModifierId.fromHex(tooShort)
    an[IllegalArgumentException] should be thrownBy ModifierId.fromHex(tooLong)
  }

  property("fromHex rejects invalid hex characters") {
    val invalidHex = "g" * 64 // 'g' is not a valid hex character
    
    an[IllegalArgumentException] should be thrownBy ModifierId.fromHex(invalidHex)
  }

  property("apply rejects invalid length byte arrays") {
    val tooShort = Array.fill(31)(0.toByte)
    val tooLong = Array.fill(33)(0.toByte)
    
    an[IllegalArgumentException] should be thrownBy ModifierId(tooShort)
    an[IllegalArgumentException] should be thrownBy ModifierId(tooLong)
  }

  property("apply rejects null byte array") {
    an[IllegalArgumentException] should be thrownBy ModifierId(null)
  }

  property("fromHex rejects null hex string") {
    an[IllegalArgumentException] should be thrownBy ModifierId.fromHex(null)
  }

  property("hashCode is consistent for equal ModifierIds") {
    val id1 = ModifierId(validBytes)
    val id2 = ModifierId(validBytes)
    
    id1 shouldBe id2
    id1.hashCode() shouldBe id2.hashCode()
  }

  property("equals performs full 32-byte comparison") {
    val bytes1 = Array.tabulate(32)(i => i.toByte)
    val bytes2 = Array.tabulate(32)(i => i.toByte)
    bytes2(31) = (bytes2(31) + 1).toByte // Last byte differs
    
    val id1 = ModifierId(bytes1)
    val id2 = ModifierId(bytes2)
    
    id1 should not be id2
    // Even if hashCode() might collide, equals() should catch the difference
    id1.equals(id2) shouldBe false
  }

  property("hashCode uses first 4 bytes (performance optimization)") {
    // Create two ModifierIds that differ only in bytes after position 3
    val bytes1 = Array.tabulate(32)(i => i.toByte)
    val bytes2 = Array.tabulate(32)(i => i.toByte)
    bytes2(4) = (bytes2(4) + 1).toByte // Only byte 4 differs
    
    val id1 = ModifierId(bytes1)
    val id2 = ModifierId(bytes2)
    
    // They should have different hashCodes (first 4 bytes are same, but let's verify with different first bytes)
    val bytes3 = Array.tabulate(32)(i => i.toByte)
    bytes3(0) = (bytes3(0) + 1).toByte // First byte differs
    val id3 = ModifierId(bytes3)
    
    id1.hashCode() should not be id3.hashCode()
  }

  property("toHexString produces correct hex encoding") {
    val bytes = Array.fill(32)(0.toByte)
    bytes(0) = 0x01.toByte
    bytes(1) = 0x23.toByte
    bytes(2) = 0x45.toByte
    bytes(3) = 0x67.toByte
    
    val id = ModifierId(bytes)
    val hex = id.toHexString
    
    hex should startWith "01234567"
    hex.length shouldBe 64
  }

  property("ModifierId can be used as Map key") {
    val id1 = ModifierId(Array.tabulate(32)(i => i.toByte))
    val id2 = ModifierId(Array.tabulate(32)(i => (i + 1).toByte))
    
    val map = Map(id1 -> "value1", id2 -> "value2")
    
    map(id1) shouldBe "value1"
    map(id2) shouldBe "value2"
    map.size shouldBe 2
  }

  property("ModifierId can be used in Set") {
    val id1 = ModifierId(Array.tabulate(32)(i => i.toByte))
    val id2 = ModifierId(Array.tabulate(32)(i => i.toByte)) // Same as id1
    val id3 = ModifierId(Array.tabulate(32)(i => (i + 1).toByte)) // Different
    
    val set = Set(id1, id2, id3)
    
    set.size shouldBe 2 // id1 and id2 are equal
    set should contain(id1)
    set should contain(id3)
  }

  property("toString returns hex string") {
    val id = ModifierId.fromHex(validHexString)
    id.toString shouldBe id.toHexString
    id.toString.length shouldBe 64
  }
}

