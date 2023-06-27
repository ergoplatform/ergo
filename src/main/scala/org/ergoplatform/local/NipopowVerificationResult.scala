package org.ergoplatform.local

/**
  * Hierarchy of outcomes when another nipopow proof is compared with one which is known to be best to the moment
  * of comparison
  */
sealed trait NipopowProofVerificationResult

/**
  * Presented nipopow proof is better than known one
  */
case object BetterChain extends NipopowProofVerificationResult

/**
  * Presented nipopow proof is no better than known one
  */
case object NoBetterChain extends NipopowProofVerificationResult

/**
  * Presented nipopow proof is not valid
  */
case object ValidationError extends NipopowProofVerificationResult


/**
  * Presented nipopow proof is starting with different genesis block
  */
case object WrongGenesis extends NipopowProofVerificationResult

