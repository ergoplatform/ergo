package org.ergoplatform.settings

import scorex.core.settings.Settings

trait ErgoSettings extends Settings {
  lazy val dataDir: String = dataDirOpt.getOrElse("/tmp/ergo")
  //todo read all from settings file

  /**
    * Keep state roothash only and validate transactions via ADProofs
    */
  val ADState: Boolean = false
  /**
    * Download block transactions and verify them (requires BlocksToKeep == 0)
    */
  val verifyTransactions: Boolean = true
  /**
    * Number of last blocks to keep with transactions and ADproofs, for all other blocks only header will be stored.
    * Keep all blocks from genesis if negative
    */
  val blocksToKeep: Int = 10
  /**
    * Download PoPoW proof on node bootstrtap
    */
  val poPoWBootstrap: Boolean = false
  /**
    * Minimal suffix size for PoPoW proof (may be pre-defined constant or settings parameter)
    */
  val minimalSuffix: Int = 10

}
