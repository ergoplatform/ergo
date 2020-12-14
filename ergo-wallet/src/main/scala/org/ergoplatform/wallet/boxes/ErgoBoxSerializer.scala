package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.SBox
import sigmastate.interpreter.VersionContext
import sigmastate.serialization.{DataSerializer, ConstantStore}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object ErgoBoxSerializer extends ErgoWalletSerializer[ErgoBox] {

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
