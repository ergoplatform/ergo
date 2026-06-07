package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height

/**
  * Interface to methods providing updating and reading height of first full block stored in local database
  */
trait MinimalFullBlockHeightFunctions {

  /**
    * @return minimal height to applu full blocks from. Its value depends on node settings,
    * if bootstrapping with UTXO set snapshot is used, the value is being set to a first block after the snapshot,
    * for other modes, if blockToKeep > 0, the value is being set to a first block of blockchain suffix
    * after headers downloaded, and the value is updated when new blocks added to the chain. If blockToKeep == 0,
    * min full block height is set to genesis block height (1).
    */
  def readMinimalFullBlockHeight(): Height

  /**
    * Update min full block height, see `readMinimalFullBlockHeight` scaladoc on how it should be done
    */
  def writeMinimalFullBlockHeight(height: Height): Unit

  /**
    * @return height (exclusive) up to which full block data has already been pruned. Used to drive
    * backlog-aware pruning: pruning may be deferred (e.g. to keep blocks the wallet has not scanned
    * yet) and resumed later from this marker. Defaults to the current minimal full block height for
    * nodes upgraded from a version that pruned everything below that height.
    */
  def readPrunedHeight(): Height

  /**
    * Update the pruned-up-to height marker, see `readPrunedHeight`
    */
  def writePrunedHeight(height: Height): Unit

}
