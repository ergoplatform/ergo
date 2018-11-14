package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.Serializer
import sigmastate.SBox
import sigmastate.serialization.DataSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.util.Try

object ErgoBoxSerializer extends Serializer[ErgoBox] {
  override def toBytes(box: ErgoBox): Array[Byte] = {
    val w = sigmastate.serialization.Serializer.startWriter()
    write(box, w)
    w.toBytes
  }

  def write(box: ErgoBox, writer: SigmaByteWriter): Unit = {
    DataSerializer.serialize[SBox.type](box, SBox, writer)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoBox] = {
    read(sigmastate.serialization.Serializer.startReader(bytes, 0))
  }

  def read(reader: SigmaByteReader): Try[ErgoBox] = Try {
    DataSerializer.deserialize(SBox, reader)
  }
}
