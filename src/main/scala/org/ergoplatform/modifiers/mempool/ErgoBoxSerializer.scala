package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.Serializer
import sigmastate.SBox
import sigmastate.serialization.DataSerializer

import scala.util.Try

object ErgoBoxSerializer extends Serializer[ErgoBox] {
  override def toBytes(obj: ErgoBox): Array[Byte] = {
    val w = sigmastate.serialization.Serializer.startWriter()
    DataSerializer.serialize[SBox.type](obj, SBox, w)
    w.toBytes
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoBox] = Try {
    val r = sigmastate.serialization.Serializer.startReader(bytes, 0)
    DataSerializer.deserialize(SBox, r)
  }
}
