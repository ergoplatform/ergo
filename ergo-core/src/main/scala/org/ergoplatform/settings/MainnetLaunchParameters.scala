package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.header.Header

/**
  * Parameters corresponding to initial moment of time in the mainnet and the testnet
  */
object MainnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

object TestnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

object DevnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter50Version),
  proposedUpdate = ErgoValidationSettingsUpdate.empty)
