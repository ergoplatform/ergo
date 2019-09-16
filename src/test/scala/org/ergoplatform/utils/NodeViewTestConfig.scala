package org.ergoplatform.utils

import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings

import scala.concurrent.duration._

case class NodeViewTestConfig(stateType: StateType,
                              verifyTransactions: Boolean) {

  def toSettings: ErgoSettings = {
    val defaultSettings = ErgoSettings.read()
    defaultSettings.copy(
      chainSettings = defaultSettings.chainSettings.copy(
        powScheme = new DefaultFakePowScheme(defaultSettings.chainSettings.powScheme.k, defaultSettings.chainSettings.powScheme.n)
      ),
      walletSettings = defaultSettings.walletSettings.copy(scanningInterval = 15.millis),
      nodeSettings = defaultSettings.nodeSettings.copy(
        stateType = stateType,
        verifyTransactions = verifyTransactions
      )
    )
  }

  override def toString: String = {
    s"State: $stateType, Verify Transactions: $verifyTransactions"
  }
}

object NodeViewTestConfig {

  def defaultConfig(config: ErgoSettings): ErgoSettings = config

  val allConfigs: List[NodeViewTestConfig] = List(
    NodeViewTestConfig(StateType.Digest, verifyTransactions = true),
    NodeViewTestConfig(StateType.Digest, verifyTransactions = false),
    NodeViewTestConfig(StateType.Utxo, verifyTransactions = true),
  )

  def verifyTxConfigs: List[NodeViewTestConfig] = NodeViewTestConfig.allConfigs.filter(_.verifyTransactions)

}
