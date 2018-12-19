package org.ergoplatform.utils

import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{ErgoSettings, NiPoPowSettings}

import scala.concurrent.duration._

case class NodeViewTestConfig(stateType: StateType,
                              verifyTransactions: Boolean,
                              popowBootstrap: Boolean) {

  def toSettings: ErgoSettings = {
    val defaultSettings = ErgoSettings.read(None)
    val niPoPowSettings = NiPoPowSettings(enabled = popowBootstrap, 3, 30, 30, 30, 0.45)
    defaultSettings.copy(
      chainSettings = defaultSettings.chainSettings.copy(
        powScheme = new DefaultFakePowScheme(defaultSettings.chainSettings.powScheme.k, defaultSettings.chainSettings.powScheme.n)
      ),
      walletSettings = defaultSettings.walletSettings.copy(scanningInterval = 15.millis),
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = stateType,
        verifyTransactions = verifyTransactions,
        poPowSettings = niPoPowSettings
      )
    )
  }

  override def toString: String = {
    s"State: $stateType, Verify Transactions: $verifyTransactions, PoPoW Bootstrap: $popowBootstrap"
  }
}

object NodeViewTestConfig {

  def defaultConfig(config: ErgoSettings): ErgoSettings = config

  val allConfigs: List[NodeViewTestConfig] = List(
    NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = true),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = false, popowBootstrap = true),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = false, popowBootstrap = false),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = false),
    NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = true),
    NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false)
  )

  def verifyTxConfigs: List[NodeViewTestConfig] = NodeViewTestConfig.allConfigs.filter(_.verifyTransactions)

}
