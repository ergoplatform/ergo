package org.ergoplatform.network

import org.ergoplatform.modifiers.history.popow.NipopowProof

object ErgoNodeViewSynchronizerProcessNipopow {
  /**
   * Command for a central node view holder component to process NiPoPoW proof,
   * and possibly initialize headers chain from a best NiPoPoW proof known, when enough proofs collected
   *
   * @param nipopowProof - proof to initialize history from
   */
  case class ProcessNipopow(nipopowProof: NipopowProof)
}
