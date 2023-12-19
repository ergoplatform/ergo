package org.ergoplatform.local

/**
  * Hierarchy of outcomes when another nipopow proof is compared with one which is known to be best to the moment
  * of comparison
  */
sealed trait NipopowProofVerificationResult

/**
  * Basic interface for valid proof verification results
  */
sealed trait CorrectNipopowProofVerificationResult extends NipopowProofVerificationResult {
  /**
    * Total number of known valid proofs verified, including this one
    */
  val totalProofsProcessed: Int
}

/**
  * Presented nipopow proof is better than known one
  */
case class BetterChain(override val totalProofsProcessed: Int) extends CorrectNipopowProofVerificationResult

/**
  * Presented nipopow proof is no better than known one
  */
case class NoBetterChain(override val totalProofsProcessed: Int) extends CorrectNipopowProofVerificationResult

/**
  * Presented nipopow proof is not valid
  */
case object ValidationError extends NipopowProofVerificationResult


/**
  * Presented nipopow proof is starting with different genesis block
  */
case object WrongGenesis extends NipopowProofVerificationResult

