package org.ergoplatform.utils

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettings, MainnetLaunchParameters, Parameters}
import org.ergoplatform.utils.ErgoCoreTestConstants.genesisStateDigest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

trait ErgoStateContextHelpers {

  def stateContext(height: Int, blockVersion: Byte, settings: ErgoSettings): ErgoStateContext = {
    val header = defaultHeaderGen.sample.get.copy(version = blockVersion, height = height)
    val params = Parameters(MainnetLaunchParameters.height,
      MainnetLaunchParameters.parametersTable.updated(Parameters.BlockVersion, blockVersion),
      MainnetLaunchParameters.proposedUpdate)
    new ErgoStateContext(Seq(header), None, genesisStateDigest, params, ErgoValidationSettings.initial,
      VotingData.empty)(settings.chainSettings)
  }

  def stateContextForTx(tx: ErgoTransaction, blockVersion: Byte, settings: ErgoSettings): ErgoStateContext = {
    stateContext(tx.outputs.map(_.creationHeight).max, blockVersion, settings: ErgoSettings)
  }

}
