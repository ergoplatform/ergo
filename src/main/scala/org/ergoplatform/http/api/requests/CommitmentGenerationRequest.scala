package org.ergoplatform.http.api.requests

import org.ergoplatform.modifiers.mempool.UnsignedErgoTransaction
import sigmastate.Values.SigmaBoolean

/**
  * Represent a request for generating signature commitments
  * for inputs of an unsigned transaction.
  *
  * @param utx - unsigned transaction
  * @param externalKeys - optionally, secrets provided by client
  */
case class CommitmentGenerationRequest(utx: UnsignedErgoTransaction,
                                       externalKeys: Option[Seq[SigmaBoolean]])
