package org.ergoplatform.network

import java.nio.ByteBuffer

trait DecodingUtils {
  def getUByte(buf: ByteBuffer): Int = getByte(buf) & 0xFF

  def getByte(buf: ByteBuffer): Byte = buf.get

  def getULong(buf: ByteBuffer): Long = {
    var result: Long = 0
    var shift = 0
    while (shift < 64) {
      val b = getByte(buf)
      result = result | ((b & 0x7F).toLong << shift)
      if ((b & 0x80) == 0) return result
      shift += 7
    }
    ???
  }

  def getBytes(buf: ByteBuffer, size: Int): Array[Byte] = {
    val res = new Array[Byte](size)
    buf.get(res)
    res
  }

  def decodeZigZagInt(n: Int): Int = {
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
    (n >>> 1) ^ -(n & 1)
  }

  def getInt(buf: ByteBuffer): Int = {
    // should only be changed simultaneously with `putInt`
    decodeZigZagInt(getULong(buf).toInt)
  }

}
