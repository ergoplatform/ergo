package scorex.db

object ByteArrayUtils {

  import java.util.Comparator

  val BYTE_ARRAY_COMPARATOR: Comparator[Array[Byte]] = (o1: Array[Byte], o2: Array[Byte]) => compare(o1, o2)

  implicit val ByteArrayOrdering: Ordering[Array[Byte]] =
    (o1: Array[Byte], o2: Array[Byte]) => ByteArrayUtils.compare(o1, o2)

  def compare(o1: Array[Byte], o2: Array[Byte]): Int = {
    val len = Math.min(o1.length, o2.length)
    var i = 0
    while (i < len) {
      val b1 = o1(i) & 0xFF
      val b2 = o2(i) & 0xFF
      if (b1 != b2) return b1 - b2
      i += 1
    }
    o1.length - o2.length
  }

  /** Put the given Short bytes at the given position in the buffer array. */
  @inline final def putShort(buf: Array[Byte], pos: Int, v: Short): Unit = {
    buf(pos) = (v >> 8).toByte
    buf(pos + 1) = v.toByte
  }

  /** Put the given Int bytes at the given position in the buffer array. */
  @inline final def putInt(buf: Array[Byte], pos: Int, v: Int): Unit = {
    buf(pos) = (v >> 24).toByte
    buf(pos + 1) = (v >> 16).toByte
    buf(pos + 2) = (v >> 8).toByte
    buf(pos + 3) = v.toByte
  }

  /** Replicate the given value n times in the buffer starting from the given position. */
  @inline final def putReplicated(buf: Array[Byte], pos: Int, n: Int, v: Byte): Unit = {
    val limit = pos + n
    var i = pos
    while(i < limit) {
      buf(i) = v
      i += 1
    }
  }

  /** Put the given bytes at the given position in the buffer array. */
  @inline final def putBytes(buf: Array[Byte], pos: Int, bytes: Array[Byte]): Unit = {
    val limit = bytes.length
    var i = 0
    while(i < limit) {
      buf(pos + i) = bytes(i)
      i += 1
    }
  }

}
