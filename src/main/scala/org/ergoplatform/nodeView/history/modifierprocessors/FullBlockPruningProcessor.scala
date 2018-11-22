package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.util.ScorexLogging

/**
  * Class that keeps and calculates minimal height for full blocks starting from which we need to download these full
  * blocks from the network and keep them in our history.
  */
class FullBlockPruningProcessor(config: NodeConfigurationSettings) extends ScorexLogging {

  @volatile private[history] var isHeadersChainSyncedVar: Boolean = false
  @volatile private[history] var minimalFullBlockHeightVar: Int = 0

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

  /** Nearest snapshot height node can use for fast syncing (penultimate snapshot).
    * @param height - height of last block
    * */
  def nearestSnapshotHeight(height: Int): Int = {
    val snapshotMaxHeight = height - config.blocksToKeep
    snapshotMaxHeight - (snapshotMaxHeight % config.snapshotCreationInterval) - config.snapshotCreationInterval
  }

  /** Update minimal full block height and header chain synced flag
    *
    * @param header - header of new best full block
    * @return minimal height to process best full block
    */
  def updateBestFullBlock(header: Header): Int = {
    minimalFullBlockHeightVar = if (config.blocksToKeep < 0) {
      0 // keep all blocks in history
    } else if (!config.stateType.requireProofs) {
      // just synced with the headers chain in pruned full mode -
      // start from height of the penultimate state snapshot available + 1.
      val snapshotHeight = nearestSnapshotHeight(header.height)
      if (snapshotHeight >= config.snapshotCreationInterval * 2) snapshotHeight + 1 else 0
    } else {
      // Start from config.blocksToKeep blocks back
      Math.max(minimalFullBlockHeight, header.height - config.blocksToKeep + 1)
    }
    if (!isHeadersChainSynced) {
      log.info(s"Headers chain is synced after header ${header.encodedId} at height ${header.height}")
      isHeadersChainSyncedVar = true
    }
    minimalFullBlockHeightVar
  }

}
