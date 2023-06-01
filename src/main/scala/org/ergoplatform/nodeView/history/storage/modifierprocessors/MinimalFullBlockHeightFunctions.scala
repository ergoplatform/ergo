package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.nodeView.history.ErgoHistory.Height

trait MinimalFullBlockHeightFunctions {
  // minimal height to applu full blocks from
  // its value depends on node settings,
  // if download with UTXO set snapshot is used, the value is being set to a first block after the snapshot,
  // if blockToKeep > 0, the value is being set to a first block of blockchain suffix after headers downloaded

  def writeMinimalFullBlockHeight(height: Height): Unit

  def readMinimalFullBlockHeight(): Height

}
