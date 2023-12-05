package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistoryConstants._
import org.ergoplatform.settings.ErgoSettings

/**
  * A class that keeps and calculates minimal height for full blocks starting from which we need to download these full
  * blocks from the network and keep them in our history.
  */
trait FullBlockPruningProcessor extends MinimalFullBlockHeightFunctions {

  protected def settings: ErgoSettings

  private def nodeConfig = settings.nodeSettings
  private def chainSettings = settings.chainSettings

  private def VotingEpochLength = chainSettings.voting.votingLength

  @volatile private[history] var isHeadersChainSyncedVar: Boolean = false

  private def extensionWithParametersHeight(height: Int): Int = {
    require(height >= VotingEpochLength)
    height - (height % VotingEpochLength)
  }

  /** Whether headers chain is synchronized with the network and full blocks could be downloaded.
    * `true` if we estimate that our chain is synced with the network.
    * Full blocks downloading is to be started after that.
    */
  def isHeadersChainSynced: Boolean = isHeadersChainSyncedVar

  /** Start height to download full blocks from
    */
  def minimalFullBlockHeight: Int = readMinimalFullBlockHeight()

  /** Check if headers chain is synchronized with the network and modifier is not too old
    */
  def shouldDownloadBlockAtHeight(height: Int): Boolean = {
    isHeadersChainSynced && height >= minimalFullBlockHeight
  }

  /** Update minimal full block height and header chain synced flag
    *
    * @param header - header of new best full block
    * @return minimal height to process best full block
    */
  def updateBestFullBlock(header: Header): Int = {
    val minimalFullBlockHeight = if (nodeConfig.blocksToKeep < 0) {
      if (nodeConfig.utxoSettings.utxoBootstrap) {
        // we have constant min full block height corresponding to first block after utxo set snapshot
        readMinimalFullBlockHeight()
      } else {
        GenesisHeight // keep all blocks in history as no pruning set
      }
    } else {
      // Start from config.blocksToKeep blocks back
      val h = Math.max(readMinimalFullBlockHeight(), header.height - nodeConfig.blocksToKeep + 1)
      // ... but not later than the beginning of a voting epoch
      if (h > VotingEpochLength) {
        Math.min(h, extensionWithParametersHeight(h))
      } else {
        h
      }
    }
    if (!isHeadersChainSynced) isHeadersChainSyncedVar = true
    writeMinimalFullBlockHeight(minimalFullBlockHeight)
    minimalFullBlockHeight
  }

}
