package org.ergoplatform.wallet.secrets

import scodec.bits.ByteVector

object Index {

  val HardRangeStart = 0x80000000

  def hardIndex(i: Int): Int = i | HardRangeStart

  def isHardened(i: Int): Boolean = (i & HardRangeStart) != 0

  def serializeIndex(i: Int): Array[Byte] = ByteVector.fromInt(i).toArray

  def parseIndex(xs: Array[Byte]): Int = ByteVector(xs).toInt(signed = false)

}
