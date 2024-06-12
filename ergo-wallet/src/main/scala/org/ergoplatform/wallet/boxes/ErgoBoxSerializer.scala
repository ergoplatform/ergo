package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}
import sigma.serialization.ConstantStore
import sigma.serialization.{SigmaByteReader, SigmaByteWriter}

object ErgoBoxSerializer extends ErgoWalletSerializer[ErgoBox] {

  override def serialize(box: ErgoBox, w: Writer): Unit = {
    val writer = new SigmaByteWriter(w, None)
    ErgoBox.sigmaSerializer.serialize(box, writer)
  }

  override def parse(r: Reader): ErgoBox = {
    val reader = new SigmaByteReader(r, new ConstantStore(), resolvePlaceholdersToConstants = false)
    ErgoBox.sigmaSerializer.parse(reader)
  }

}
