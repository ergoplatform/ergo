package org.ergoplatform.settings

import org.ergoplatform.ErgoBox
import org.ergoplatform.reemission.ReemissionRules
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import scorex.util.ModifierId
import scorex.util.encode.Base16

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
                              injectionBoxBytesEncoded: ModifierId) {

  val emissionNftIdBytes: Array[Byte] = Algos.decode(emissionNftId).get
  val reemissionNftIdBytes: Array[Byte] = Algos.decode(reemissionNftId).get
  val reemissionTokenIdBytes: Array[Byte] = Algos.decode(reemissionTokenId).get

  lazy val InjectionBoxBytes: Array[Byte] = Base16.decode(injectionBoxBytesEncoded).get
  lazy val injectionBox: ErgoBox = ErgoBoxSerializer.parseBytes(InjectionBoxBytes)

  val reemissionRules = new ReemissionRules(reemissionSettings = this)

}
