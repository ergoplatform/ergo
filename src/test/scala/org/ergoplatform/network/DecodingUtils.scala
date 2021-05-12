package org.ergoplatform.network

import java.nio.ByteBuffer

// Common functions used in parsers, collected in one place to be easily understood and rewritten into other languages
trait DecodingUtils {

  /**
    * Read unsigned byte
    */
  def getUByte(buf: ByteBuffer): Int = getByte(buf) & 0xFF

  /**
    * Read signed byte
    */
  def getByte(buf: ByteBuffer): Byte = buf.get

  /**
    * Reading up to 64 bits VLQ-encoded value. The value is considered to be signed.
    * Can be read less than 64 bytes!
    * See https://en.wikipedia.org/wiki/Variable-length_quantity
    */
  def getULong(buf: ByteBuffer): Long = {
    var result: Long = 0
    var shift = 0
    while (shift < 64) {
      val b = getByte(buf)
      result = result | ((b & 0x7F).toLong << shift)
      if ((b & 0x80) == 0) return result
      shift += 7
    }
    throw new IllegalStateException("Trying to read long, but more bytes than needed found")
  }

  /**
    * Read signed int (32 bits) value
    **/
  def getInt(buf: ByteBuffer): Int = {
    // should only be changed simultaneously with `putInt`
    decodeZigZagInt(getULong(buf).toInt)
  }


  /**
    * Read `size` bytes
    */
  def getBytes(buf: ByteBuffer, size: Int): Array[Byte] = {
    val res = new Array[Byte](size)
    buf.get(res)
    res
  }

  /**
    * Decode ZigZag-encoded (32 bits) integer
    */
  def decodeZigZagInt(n: Int): Int = {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
    (n >>> 1) ^ -(n & 1)
  }

}
