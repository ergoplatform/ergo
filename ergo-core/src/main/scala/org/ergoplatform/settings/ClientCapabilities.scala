package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.StateType

/**
 * Features client may have enabled, they are reported to other peers
 */
trait ClientCapabilities {
  val stateType: StateType
  val verifyTransactions: Boolean
  val blocksToKeep: Int
  val utxoSettings: UtxoSettings
  val nipopowSettings: NipopowSettings
}