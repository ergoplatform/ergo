package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.NodeConfigurationSettings

/**
  * Class that keeps and calculates minimal height for full blocks starting from which we need to download these full
  * blocks from the network and keep them in our history.
  */
class FullBlockPruningProcessor(config: NodeConfigurationSettings) {

  @volatile private[history] var isHeadersChainSyncedVar: Boolean = false
  @volatile private[history] var minimalFullBlockHeightVar: Int = ErgoHistory.GenesisHeight

  /** Whether headers chain is synchronized with the network and full blocks could be downloaded.
    * `true` if we estimate, that our chain is synced with the network. Start downloading full blocks after that
    */
  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  /** Start height to download full blocks
    */
  def minimalFullBlockHeight: Int = minimalFullBlockHeightVar

  /** Check if headers chain is synchronized with the network and modifier is not too old
    */
  def shouldDownloadBlockAtHeight(height: Int): Boolean = {
    isHeadersChainSynced && minimalFullBlockHeight <= height
  }

  /** Update minimal full block height and header chain synced flag
    *
    * @param header - header of new best full block
    * @return minimal height to process best full block
    */
  def updateBestFullBlock(header: Header): Int = {
    minimalFullBlockHeightVar = if (config.blocksToKeep < 0) {
      ErgoHistory.GenesisHeight // keep all blocks in history
    } else if (!isHeadersChainSynced && !config.stateType.requireProofs) {
      // just synced with the headers chain - determine first full block to apply
      ErgoHistory.GenesisHeight //TODO start with the height of UTXO snapshot applied. Start from genesis util this is implemented
    } else {
      // Start from config.blocksToKeep blocks back
      Math.max(minimalFullBlockHeight, header.height - config.blocksToKeep + 1)
    }
    if (!isHeadersChainSynced) isHeadersChainSyncedVar = true
    minimalFullBlockHeightVar
  }

}
