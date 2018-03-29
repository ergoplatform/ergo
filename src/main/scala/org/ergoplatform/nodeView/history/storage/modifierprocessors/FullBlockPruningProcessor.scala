package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.NodeConfigurationSettings

/**
  * Class that keeps and calculates minimal height for full blocks starting from which we need to download these full
  * blocks from the network and keep them in our history.
  */
class FullBlockPruningProcessor(config: NodeConfigurationSettings) {

  private[history] var minimalFullBlockHeightVar: Int = Int.MaxValue

  /**
    * Start height to download full blocks.
    * Int.MaxValue when headers chain is not synchronized with the network and no full blocks download needed
    */
  def minimalFullBlockHeight: Int = minimalFullBlockHeightVar

  /**
    * Update minimal full block height
    *
    * @param header - header of new best full block
    * @return minimal height to process best full block
    */
  def updateBestFullBlock(header: Header): Int = {
    minimalFullBlockHeightVar = minimalFullBlockHeightAfter(header)
    minimalFullBlockHeightVar
  }

  private def minimalFullBlockHeightAfter(header: Header): Int = {
    if (!config.verifyTransactions) {
      // we do not verify transactions at any height
      Int.MaxValue
    } else if (minimalFullBlockHeightVar == Int.MaxValue) {
      // just synced with the headers chain - determine first full block to apply
      if (config.blocksToKeep < 0) {
        // keep all blocks in history
        0
      } else if (!config.stateType.requireProofs) {
        //TODO start with the height of UTXO snapshot applied. Start from genesis util this is implemented
        0
      } else {
        // Start from config.blocksToKeep blocks back
        Math.max(0, header.height - config.blocksToKeep + 1)
      }
    } else if (config.blocksToKeep >= 0) {
      Math.max(header.height - config.blocksToKeep + 1, minimalFullBlockHeightVar)
    } else {
      0
    }
  }

}
