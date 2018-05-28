package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.Serializer

import scala.util.Try

object ErgoBoxSerializer extends Serializer[ErgoBox] {
  override def toBytes(obj: ErgoBox): Array[Byte] = ErgoBox.serializer.toBytes(obj)

  override def parseBytes(bytes: Array[Byte]): Try[ErgoBox] = ErgoBox.serializer.parseBytes(bytes)
}
