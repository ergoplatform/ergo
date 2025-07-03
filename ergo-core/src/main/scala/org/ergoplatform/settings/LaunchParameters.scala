package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.header.Header

/**
  * Parameters corresponding to initial moment of time in the mainnet and the testnet
  */
object MainnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

/**
  * Parameters corresponding to the genesis block in the public testnet
  */
object TestnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

/**
  * Initial parameters corresponding to a devnet which is starting with 5.0 activated
  */
object DevnetLaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter50Version),
  proposedUpdate = ErgoValidationSettingsUpdate.empty)

/**
  * Initial parameters corresponding to a devnet which is starting with 6.0 activated
  */
object Devnet60LaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters.updated(Parameters.BlockVersion, Header.Interpreter60Version),
  proposedUpdate = ErgoValidationSettingsUpdate.empty)
