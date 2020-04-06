package org.ergoplatform.utils

import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings

import scala.concurrent.duration._

case class NodeViewTestConfig(stateType: StateType,
                              verifyTransactions: Boolean,
                              popowBootstrap: Boolean) {

  def toSettings: ErgoSettings = {
    val defaultSettings = ErgoSettings.read()
    defaultSettings.copy(
      chainSettings = defaultSettings.chainSettings.copy(
        powScheme = new DefaultFakePowScheme(defaultSettings.chainSettings.powScheme.k, defaultSettings.chainSettings.powScheme.n)
      ),
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = stateType,
        verifyTransactions = verifyTransactions,
        poPoWBootstrap = popowBootstrap
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
