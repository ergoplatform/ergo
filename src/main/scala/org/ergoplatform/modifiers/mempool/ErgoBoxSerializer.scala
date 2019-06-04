package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object ErgoBoxSerializer extends ScorexSerializer[ErgoBox] {

  override def serialize(box: ErgoBox, w: Writer): Unit = {
    val writer = new SigmaByteWriter(w, None)
    ErgoBox.sigmaSerializer.serialize(box, writer)
  }

  override def parse(r: Reader): ErgoBox = {
    val reader = new SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false)
    ErgoBox.sigmaSerializer.parse(reader)
  }
}
