package org.ergoplatform.mining.difficulty

import java.math.BigInteger

import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * Binary serializer and (en/de)coding utils for difficulty presented in different formats (as a big integer or nbits
  * encoded 32 bits number)
  */
object DifficultySerializer extends ErgoSerializer[NBits] {

  override def serialize(obj: NBits, w: Writer): Unit = {
    w.putBytes(uint32ToByteArrayBE(obj))
  }

  override def parse(r: Reader): NBits = {
    readUint32BE(r.getBytes(4))
  }

  /**
    * <p>The "compact" format is a representation of a whole number N using an unsigned 32 bit number similar to a
    * floating point format. The most significant 8 bits are the unsigned exponent of base 256. This exponent can
    * be thought of as "number of bytes of N". The lower 23 bits are the mantissa. Bit number 24 (0x800000) represents
    * the sign of N. Therefore, N = (-1^sign) * mantissa * 256^(exponent-3).</p>
    *
    * <p>Satoshi's original implementation used BN_bn2mpi() and BN_mpi2bn(). MPI uses the most significant bit of the
    * first byte as sign. Thus 0x1234560000 is compact 0x05123456 and 0xc0de000000 is compact 0x0600c0de. Compact
    * 0x05c0de00 would be -0x40de000000.</p>
    *
    * <p>Bitcoin only uses this "compact" format for encoding difficulty targets, which are unsigned 256bit quantities.
    * Thus, all the complexities of the sign bit and using base 256 are probably an implementation accident.</p>
    */
  def decodeCompactBits(compact: Long): BigInt = {
    val size: Int = (compact >> 24).toInt & 0xFF
    val bytes: Array[Byte] = new Array[Byte](4 + size)
    bytes(3) = size.toByte
    if (size >= 1) bytes(4) = ((compact >> 16) & 0xFF).toByte
    if (size >= 2) bytes(5) = ((compact >> 8) & 0xFF).toByte
    if (size >= 3) bytes(6) = (compact & 0xFF).toByte
    decodeMPI(bytes)
  }

  /**
    * @see Utils#decodeCompactBits(long)
    */
  def encodeCompactBits(requiredDifficulty: BigInt): Long = {
    val value = requiredDifficulty.bigInteger
    var result: Long = 0L
    var size: Int = value.toByteArray.length
    if (size <= 3) {
      result = value.longValue << 8 * (3 - size)
    } else {
      result = value.shiftRight(8 * (size - 3)).longValue
    }
    // The 0x00800000 bit denotes the sign.
    // Thus, if it is already set, divide the mantissa by 256 and increase the exponent.
    if ((result & 0x00800000L) != 0) {
      result >>= 8
      size += 1
    }
    result |= size << 24
    val a: Int = if (value.signum == -1) 0x00800000 else 0
    result |= a
    result
  }

  /** Parse 4 bytes from the byte array (starting at the offset) as unsigned 32-bit integer in big endian format. */
  def readUint32BE(bytes: Array[Byte]): Long = ((bytes(0) & 0xffL) << 24) | ((bytes(1) & 0xffL) << 16) | ((bytes(2) & 0xffL) << 8) | (bytes(3) & 0xffL)

  def uint32ToByteArrayBE(value: Long): Array[Byte] = {
    Array(0xFF & (value >> 24), 0xFF & (value >> 16), 0xFF & (value >> 8), 0xFF & value).map(_.toByte)
  }

  /**
    * MPI encoded numbers are produced by the OpenSSL BN_bn2mpi function. They consist of
    * a 4 byte big endian length field, followed by the stated number of bytes representing
    * the number in big endian format (with a sign bit).
    */
  @SuppressWarnings(Array("NullAssignment"))
  private def decodeMPI(mpi: Array[Byte]): BigInteger = {
    var buf: Array[Byte] = null // scalastyle:ignore

    val length: Int = readUint32BE(mpi).toInt
    buf = new Array[Byte](length)
    System.arraycopy(mpi, 4, buf, 0, length)

    if (buf.length == 0) {
      BigInteger.ZERO
    } else {
      val isNegative: Boolean = (buf(0) & 0x80) == 0x80
      if (isNegative) buf(0) = (buf(0) & 0x7f).toByte
      val result: BigInteger = new BigInteger(buf)
      if (isNegative) {
        result.negate
      } else {
        result
      }
    }
  }
}
