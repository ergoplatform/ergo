package org.ergoplatform.modifiers.mempool

import org.ergoplatform.ErgoBox
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.interpreter.VersionContext
import sigmastate.serialization.ConstantStore
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object ErgoBoxSerializer extends ScorexSerializer[ErgoBox] {

  override def serialize(box: ErgoBox, w: Writer): Unit = {
    val writer = new SigmaByteWriter(w, None)
    ErgoBox.sigmaSerializer.serialize(box, writer)
  }

  override def parse(r: Reader): ErgoBox = {
    // TODO v4.0: obtain the ACTUAL versions
    val versionContext = VersionContext(0) // v3.x

    val reader = new SigmaByteReader(r,
      new ConstantStore(),
      resolvePlaceholdersToConstants = false,
      versionContext)

    ErgoBox.sigmaSerializer.parse(reader)
  }
}
