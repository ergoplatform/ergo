package org.ergoplatform.http.api.requests

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import sigmastate.Values.SigmaBoolean

/**
  * Represent a request for extracting prover hints from transaction.
  *
  * @param tx - transaction
  * @param real - real signers of transaction inputs
  * @param simulated - simulated signers of transaction inputs
  */
case class HintExtractionRequest(tx: ErgoTransaction,
                                 real: Seq[SigmaBoolean],
                                 simulated: Seq[SigmaBoolean])
