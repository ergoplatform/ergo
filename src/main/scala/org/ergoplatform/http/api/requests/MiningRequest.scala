package org.ergoplatform.http.api.requests

import org.ergoplatform.modifiers.mempool.ErgoTransaction

/**
  * Represents a request to generate a candidate with the given transactions and miner public key.
  *
  * @param txs      Transactions to include in the block candidate
  * @param pk       String Hexadecimal representation of public key to use as minerPk
  */
case class MiningRequest(txs: Seq[ErgoTransaction], pk: String)
