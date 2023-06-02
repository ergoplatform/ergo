package org.ergoplatform.settings

import org.ergoplatform.ErgoBox
import org.ergoplatform.reemission.ReemissionRules
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.eval.Extensions.ArrayOps
import special.collection.Coll

/**
  * Configuration section for re-emission (EIP27) parameters
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class ReemissionSettings(checkReemissionRules: Boolean,
                              emissionNftId: ModifierId,
                              reemissionTokenId: ModifierId,
                              reemissionNftId: ModifierId,
                              activationHeight: Int,
                              reemissionStartHeight: Int,
                              injectionBoxBytesEncoded: String) {

  val emissionNftIdBytes: Coll[Byte] = emissionNftId.toBytes.toColl
  val reemissionNftIdBytes: Coll[Byte] = reemissionNftId.toBytes.toColl
  val reemissionTokenIdBytes: Coll[Byte] = reemissionTokenId.toBytes.toColl

  lazy val InjectionBoxBytes: Array[Byte] = Base16.decode(injectionBoxBytesEncoded).get
  lazy val injectionBox: ErgoBox = ErgoBoxSerializer.parseBytes(InjectionBoxBytes)

  val reemissionRules = new ReemissionRules(reemissionSettings = this)

}
