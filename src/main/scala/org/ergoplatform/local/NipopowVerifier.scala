package org.ergoplatform.local

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.popow.NipopowProof
import scorex.util.ModifierId

/**
  * A verifier for PoPoW proofs. During its lifetime, it processes many proofs with the aim of deducing at any given
  * point what is the best (sub)chain rooted at the specified genesis.
  *
  * @param genesisIdOpt    - the block id of the genesis block as hard-coded in config. If not available, proof for
  *                        any chain could be accepted! This is not desirable likely, thus better to set genesis block
  *                        id in settings!
  *
  */
class NipopowVerifier(genesisIdOpt: Option[ModifierId]) {

  private var bestProofOpt: Option[NipopowProof] = None
  private var proofsProcessed = 0

  def bestChain: Seq[Header] = bestProofOpt.synchronized {
    bestProofOpt.map(_.headersChain).getOrElse(Seq())
  }

  /**
    * Process a NiPoPoW proof, replace current best proof known with it if it is valid and better than
    * the best proof.
    * @return - status of newProof validation, see `NipopowProofVerificationResult` ScalaDoc
    */
  def process(newProof: NipopowProof): NipopowProofVerificationResult = bestProofOpt.synchronized {
    if (genesisIdOpt.isEmpty || genesisIdOpt.contains(newProof.headersChain.head.id)) {
      bestProofOpt match {
        case Some(bestProof) =>
          if (newProof.isBetterThan(bestProof)) {
            bestProofOpt = Some(newProof)
            proofsProcessed += 1
            BetterChain(proofsProcessed)
          } else {
            if (newProof.isValid) {
              proofsProcessed += 1
              NoBetterChain(proofsProcessed)
            } else {
              ValidationError
            }
          }
        case None =>
          if (newProof.isValid) {
            bestProofOpt = Some(newProof)
            proofsProcessed += 1
            BetterChain(proofsProcessed)
          } else {
            ValidationError
          }
      }
    } else {
      WrongGenesis
    }
  }

  /**
    * Clear proof stored
   */
  def reset(): Unit = bestProofOpt.synchronized {
    bestProofOpt = None
  }

}
