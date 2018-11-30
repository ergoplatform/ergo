package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.SBox
import sigmastate.serialization.DataSerializer

object ErgoBoxSerializer extends ScorexSerializer[ErgoBox] {

  override def serialize(box: ErgoBox, w: Writer): Unit = {
    DataSerializer.serialize[SBox.type](box, SBox, w)
  }

  override def parse(r: Reader): ErgoBox = {
    DataSerializer.deserialize(SBox, r)
  }
}
