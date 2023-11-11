package org.ergoplatform.reemission

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.contracts.ReemissionContracts
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.settings.ReemissionSettings
import sigma.Coll


/**
  * Contains re-emission contracts (defined in `ReemissionContracts`) and helper functions
  * for re-emission.
  */
class ReemissionRules(reemissionSettings: ReemissionSettings) extends ReemissionContracts {

  override val reemissionNftIdBytes: Coll[Byte] = reemissionSettings.reemissionNftIdBytes
  override val reemissionStartHeight: Height = reemissionSettings.reemissionStartHeight

  /**
    * How many ERG taken from emission to re-emission initially
    */
  val basicChargeAmount = 12 // in ERG

  /**
    * @return how many re-emission tokens can be unlocked at given height
    */
  def reemissionForHeight(height: Height,
                          emissionRules: EmissionRules): Long = {
    val emission = emissionRules.emissionAtHeight(height)
    if (height >= reemissionSettings.activationHeight &&
      emission >= (basicChargeAmount + 3) * EmissionRules.CoinsInOneErgo) {
      basicChargeAmount * EmissionRules.CoinsInOneErgo
    } else if (height >= reemissionSettings.activationHeight &&
      emission > 3 * EmissionRules.CoinsInOneErgo) {
      emission - 3 * EmissionRules.CoinsInOneErgo
    } else {
      0L
    }
  }
}