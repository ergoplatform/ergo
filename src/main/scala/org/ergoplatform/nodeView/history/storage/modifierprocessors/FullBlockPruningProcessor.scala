package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.NodeConfigurationSettings

/**
  * Class that keeps and calculates minimal height for full blocks to keep in history
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
      Int.MaxValue
    } else if (config.blocksToKeep >= 0 && minimalFullBlockHeightVar == Int.MaxValue) {
      Math.max(0, header.height - config.blocksToKeep + 1)
    } else if (config.blocksToKeep >= 0) {
      Math.max(Math.max(0, header.height - config.blocksToKeep + 1), minimalFullBlockHeightVar)
    } else {
      0
    }
  }

}
