package org.ergoplatform.settings

import org.ergoplatform.ErgoBox
import org.ergoplatform.reemission.ReemissionRules
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.utils.Extensions.ModifierIdOps
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

  val emissionNftIdBytes: Coll[Byte] = emissionNftId.toColl
  val reemissionNftIdBytes: Coll[Byte] = reemissionNftId.toColl
  val reemissionTokenIdBytes: Coll[Byte] = reemissionTokenId.toColl

  lazy val InjectionBoxBytes: Array[Byte] = Base16.decode(injectionBoxBytesEncoded).get
  lazy val injectionBox: ErgoBox = ErgoBoxSerializer.parseBytes(InjectionBoxBytes)

  val reemissionRules = new ReemissionRules(reemissionSettings = this)

}
