package org.ergoplatform.modifiers

import java.util.Arrays

/**
 * Efficient wrapper around Array[Byte] for cryptographic hash identifiers.
 * 
 * This class provides correct hashCode and equals implementations for use in
 * Map and Set collections, while maintaining better performance than String-based
 * implementations.
 * 
 * IMPORTANT: This class is fully immutable. All construction methods perform
 * defensive copying, and all methods that expose bytes return copies.
 * 
 * @param hashBytes The 32-byte cryptographic hash backing this ModifierId (internal, never exposed)
 */
class ModifierId private (private val hashBytes: Array[Byte]) {
  require(hashBytes != null, "hashBytes cannot be null")
  require(hashBytes.length == ErgoNodeViewModifier.ModifierIdSize, 
    s"ModifierId must be exactly ${ErgoNodeViewModifier.ModifierIdSize} bytes, got ${hashBytes.length}")

  /**
   * Efficient hashCode implementation using only the first 4 bytes.
   * 
   * PERFORMANCE TRADEOFF: We use only 4 bytes instead of hashing all 32 bytes because:
   * - ModifierId is used extensively as cache keys (Map/Set operations)
   * - The first 4 bytes of cryptographic hashes provide excellent randomness
   * - This reduces hashCode computation time by ~8x (4 bytes vs 32 bytes)
   * 
   * CORRECTNESS: Hash collisions are acceptable because:
   * - equals() performs full 32-byte comparison, ensuring correctness
   * - Cryptographic hashes have excellent distribution in the first 4 bytes
   * - The performance benefit outweighs the negligible collision risk
   * 
   * This is a deliberate performance optimization for cache-heavy code paths.
   */
  override def hashCode(): Int = {
    // Use first 4 bytes to create an Int (big-endian)
    // No need for length check since we require exactly 32 bytes in constructor
    ((hashBytes(0) & 0xFF) << 24) |
    ((hashBytes(1) & 0xFF) << 16) |
    ((hashBytes(2) & 0xFF) << 8) |
    (hashBytes(3) & 0xFF)
  }

  /**
   * Efficient equals implementation using Arrays.equals for byte-by-byte comparison.
   * Performs full 32-byte comparison to ensure correctness even if hashCode() collides.
   */
  override def equals(other: Any): Boolean = other match {
    case that: ModifierId => Arrays.equals(this.hashBytes, that.hashBytes)
    case _ => false
  }

  /**
   * Get the underlying byte array. Returns a defensive copy to prevent mutation.
   * 
   * IMMUTABILITY: This method always returns a new array copy. Modifying the
   * returned array will not affect this ModifierId instance.
   */
  def toBytes: Array[Byte] = {
    val copy = new Array[Byte](hashBytes.length)
    System.arraycopy(hashBytes, 0, copy, 0, hashBytes.length)
    copy
  }

  /**
   * Convert to hex-encoded string for display/serialization purposes.
   * This is lazily computed and cached.
   */
  lazy val toHexString: String = {
    val sb = new StringBuilder(hashBytes.length * 2)
    var i = 0
    while (i < hashBytes.length) {
      val b = hashBytes(i) & 0xFF
      if (b < 16) sb.append('0')
      sb.append(Integer.toHexString(b))
      i += 1
    }
    sb.toString()
  }

  /**
   * String representation for display purposes.
   */
  override def toString: String = toHexString

  /**
   * Convert to Coll[Byte] for sigma compatibility.
   * This method provides compatibility with the old ModifierIdOps extension.
   * 
   * IMMUTABILITY: Returns a Coll created from a defensive copy of the internal bytes.
   */
  def toColl: sigma.Coll[Byte] = {
    import sigma.Colls
    // Create defensive copy before passing to Colls.fromArray to ensure immutability
    val copy = new Array[Byte](hashBytes.length)
    System.arraycopy(hashBytes, 0, copy, 0, hashBytes.length)
    Colls.fromArray(copy)
  }
}

object ModifierId {
  /**
   * Create a ModifierId from a byte array.
   * 
   * IMMUTABILITY: The input array is defensively copied. Modifying the original
   * array after calling this method will not affect the returned ModifierId.
   * 
   * @param hashBytes Must be exactly 32 bytes (ErgoNodeViewModifier.ModifierIdSize)
   * @throws IllegalArgumentException if array length is not exactly 32 bytes
   */
  def apply(hashBytes: Array[Byte]): ModifierId = {
    require(hashBytes != null, "hashBytes cannot be null")
    require(hashBytes.length == ErgoNodeViewModifier.ModifierIdSize,
      s"ModifierId must be exactly ${ErgoNodeViewModifier.ModifierIdSize} bytes, got ${hashBytes.length}")
    // Defensive copy to ensure immutability
    val copy = new Array[Byte](hashBytes.length)
    System.arraycopy(hashBytes, 0, copy, 0, hashBytes.length)
    new ModifierId(copy)
  }

  /**
   * Create a ModifierId from a hex-encoded string.
   * 
   * @param hexString Must be exactly 64 hex characters (32 bytes * 2)
   * @throws IllegalArgumentException if string length is incorrect or contains invalid hex characters
   */
  def fromHex(hexString: String): ModifierId = {
    require(hexString != null, "hexString cannot be null")
    require(hexString.length == ErgoNodeViewModifier.ModifierIdSize * 2,
      s"Hex string must be exactly ${ErgoNodeViewModifier.ModifierIdSize * 2} characters, got ${hexString.length}")
    
    val bytes = new Array[Byte](ErgoNodeViewModifier.ModifierIdSize)
    var i = 0
    var j = 0
    while (i < hexString.length) {
      val high = Character.digit(hexString.charAt(i), 16)
      val low = Character.digit(hexString.charAt(i + 1), 16)
      if (high < 0 || low < 0) {
        throw new IllegalArgumentException(s"Invalid hex character at position $i in hex string")
      }
      bytes(j) = ((high << 4) | low).toByte
      i += 2
      j += 1
    }
    new ModifierId(bytes)
  }

  /**
   * Convert ModifierId to byte array (defensive copy).
   * 
   * @return A new byte array copy. Modifying it will not affect the original ModifierId.
   */
  def toBytes(id: ModifierId): Array[Byte] = id.toBytes

  /**
   * Convert byte array to ModifierId.
   * 
   * IMMUTABILITY: The input array is defensively copied.
   * 
   * @param bytes Must be exactly 32 bytes
   * @throws IllegalArgumentException if array length is not exactly 32 bytes
   */
  def fromBytes(bytes: Array[Byte]): ModifierId = ModifierId(bytes)
}

