package org.ergoplatform.utils

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.{ErgoStateContext, VotingData}
import org.ergoplatform.settings.{ErgoSettings, ErgoValidationSettings, MainnetLaunchParameters, Parameters}
import org.ergoplatform.utils.ErgoCoreTestConstants.genesisStateDigest
import org.ergoplatform.utils.generators.ErgoCoreGenerators.defaultHeaderGen

/**
  * Used in tests, state context generation utils
  */
trait ErgoStateContextHelpers {

  /**
    * Creates an ErgoStateContext for testing purposes at a specific height and block version.
    *
    * @param height The block height for the state context
    * @param blockVersion The block version to use in parameters
    * @param settings Ergo settings containing chain configuration
    * @return An ErgoStateContext instance with the specified parameters, containing a default header
    *         at the given height and version, using mainnet launch parameters with the specified
    *         block version, and initial validation settings.
    */
  def stateContext(height: Int, blockVersion: Byte, settings: ErgoSettings): ErgoStateContext = {
    val header = defaultHeaderGen.sample.get.copy(version = blockVersion, height = height)
    val params = Parameters(MainnetLaunchParameters.height,
      MainnetLaunchParameters.parametersTable.updated(Parameters.BlockVersion, blockVersion),
      MainnetLaunchParameters.proposedUpdate)
    new ErgoStateContext(Seq(header), None, genesisStateDigest, params, ErgoValidationSettings.initial,
      VotingData.empty)(settings.chainSettings)
  }

  /**
    * Creates an ErgoStateContext for testing purposes specifically for a given transaction.
    * The state context is created at the maximum creation height of the transaction's outputs,
    * using the specified block version and settings.
    *
    * @param tx The transaction for which to create the state context
    * @param blockVersion The block version to use in parameters
    * @param settings Ergo settings containing chain configuration
    * @return An ErgoStateContext instance created at the maximum output creation height
    *         of the given transaction, with the specified block version and settings.
    */
  def stateContextForTx(tx: ErgoTransaction, blockVersion: Byte, settings: ErgoSettings): ErgoStateContext = {
    stateContext(tx.outputs.map(_.creationHeight).max, blockVersion, settings: ErgoSettings)
  }

}
