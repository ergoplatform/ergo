package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

/**
  * ADProof processor for regime that validate transactions via ADProofs
  */
trait ADStateProofsProcessor extends FullProofsProcessor {

  protected val adState: Boolean = true

}
