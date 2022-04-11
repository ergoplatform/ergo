package org.ergoplatform.settings

/**
  * Parameters corresponding to initial moment of time in the mainnet and the testnet
  */
object LaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  eip27Supported = false,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)
