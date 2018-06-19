package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

/**
  * ADProof processor for regime that validate transactions via full state
  * and collect ADProofs to send them to light nodes
  */
trait FullStateProofsProcessor extends FullProofsProcessor {

  protected val adState: Boolean = false

}
