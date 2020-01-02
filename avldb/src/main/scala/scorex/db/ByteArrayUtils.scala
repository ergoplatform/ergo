package scorex.db

object ByteArrayUtils {

  import java.util.Comparator

  val BYTE_ARRAY_COMPARATOR: Comparator[Array[Byte]] = (o1: Array[Byte], o2: Array[Byte]) => compare(o1, o2)

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
}
