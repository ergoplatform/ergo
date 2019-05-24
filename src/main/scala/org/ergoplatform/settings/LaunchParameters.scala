package org.ergoplatform.settings

import org.ergoplatform.settings.Parameters._

object LaunchParameters extends Parameters(
  height = 0,
  parametersTable = Map(
    StorageFeeFactorIncrease -> Parameters.StorageFeeFactorDefault,
    MinValuePerByteIncrease -> Parameters.MinValuePerByteDefault,
    TokenAccessCostIncrease -> Parameters.TokenAccessCostDefault,
    InputCostIncrease -> Parameters.InputCostDefault,
    DataInputCostIncrease -> Parameters.DataInputCostDefault,
    OutputCostIncrease -> Parameters.OutputCostDefault,
    MaxExtensionSizeIncrease -> Parameters.MaxExtensionSizeDefault,
    MaxBlockSizeIncrease -> Parameters.MaxBlockSizeDefault,
    MaxBlockCostIncrease -> Parameters.MaxBlockCostDefault,
    BlockVersion -> 1),
  Seq()
)
