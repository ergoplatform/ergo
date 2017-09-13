package org.ergoplatform.utils

import java.nio.{ByteBuffer, ByteOrder}

object LittleEndianBytes {
  def leIntToByteArray(i: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(Integer.SIZE / 8)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(i)
    bb.array
  }
}
