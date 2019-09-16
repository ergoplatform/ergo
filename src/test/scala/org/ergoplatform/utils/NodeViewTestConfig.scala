package org.ergoplatform.utils

import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings

import scala.concurrent.duration._

case class NodeViewTestConfig(stateType: StateType,
                              verifyTransactions: Boolean,
                              poPowBootstrap: Boolean) {

  def toSettings: ErgoSettings = {
    val defaultSettings = ErgoSettings.read()
    defaultSettings.copy(
      chainSettings = defaultSettings.chainSettings.copy(
        powScheme = new DefaultFakePowScheme(defaultSettings.chainSettings.powScheme.k, defaultSettings.chainSettings.powScheme.n)
      ),
      walletSettings = defaultSettings.walletSettings.copy(scanningInterval = 15.millis),
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = stateType,
        verifyTransactions = verifyTransactions,
        poPowSettings = defaultSettings.nodeSettings.poPowSettings.copy(bootstrap = poPowBootstrap)
      )
    )
  }

  override def toString: String = {
    s"State: $stateType, Verify Transactions: $verifyTransactions, PoPoW Bootstrap: $poPowBootstrap"
  }
}

object NodeViewTestConfig {

  def defaultConfig(config: ErgoSettings): ErgoSettings = config

  val allConfigs: List[NodeViewTestConfig] = List(
    NodeViewTestConfig(StateType.Digest, verifyTransactions = true, poPowBootstrap = true),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = false, poPowBootstrap = true),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = false, poPowBootstrap = false),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = true, poPowBootstrap = false),
    NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, poPowBootstrap = true),
    NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, poPowBootstrap = false)
  )

  def verifyTxConfigs: List[NodeViewTestConfig] = NodeViewTestConfig.allConfigs.filter(_.verifyTransactions)

}
