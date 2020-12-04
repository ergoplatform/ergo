package org.ergoplatform.http.api.requests

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import sigmastate.Values.SigmaBoolean

/**
  * Represent a request for extracting prover hints from transaction.
  *
  * @param tx - transaction
  * @param real - real signers of transaction inputs
  * @param simulated - simulated signers of transaction inputs
  * @param inputs     - hex-encoded input boxes bytes for the unsigned transaction (optional)
  * @param dataInputs - hex-encoded data-input boxes bytes for the unsigned transaction (optional)
  */
case class HintExtractionRequest(tx: ErgoTransaction,
                                 real: Seq[SigmaBoolean],
                                 simulated: Seq[SigmaBoolean],
                                 inputs: Option[Seq[String]],
                                 dataInputs: Option[Seq[String]])
