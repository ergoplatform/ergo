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
  MaxBlockSizeIncrease -> 512 * 1024,
  MaxBlockCostIncrease -> 1000000,
  BlockVersion -> 1),
  Seq()
)
