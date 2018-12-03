package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.SBox
import sigmastate.serialization.{ConstantStore, DataSerializer}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object ErgoBoxSerializer extends ScorexSerializer[ErgoBox] {

  override def serialize(box: ErgoBox, w: Writer): Unit = {
    val writer = new SigmaByteWriter(w, None)
    DataSerializer.serialize[SBox.type](box, SBox, writer)
  }

  override def parse(r: Reader): ErgoBox = {
    val reader = new SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false)
    DataSerializer.deserialize(SBox, reader)
  }
}
