package org.ergoplatform.http.api.requests

import sigmastate.Values.SigmaBoolean

/**
  * Result of reduction of ErgoTree for context provided (used in /script/executeWithContext)
  *
  * @param value - sigma-proposition (to be proven via a sigma-protocol)
  * @param cost - cost of the original script
  */
case class CryptoResult(value: SigmaBoolean, cost: Long)
