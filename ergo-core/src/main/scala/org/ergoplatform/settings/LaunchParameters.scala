package org.ergoplatform.settings

/**
  * Parameters corresponding to initial moment of time in the mainnet and the testnet
  */
object LaunchParameters extends Parameters(height = 0,
  parametersTable = Parameters.DefaultParameters,
  proposedUpdate = ErgoValidationSettingsUpdate.empty)
