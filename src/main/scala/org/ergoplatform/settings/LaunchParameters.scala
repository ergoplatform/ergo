package org.ergoplatform.settings

import org.ergoplatform.settings.Parameters._

object LaunchParameters extends Parameters(height = 0, parametersTable = Map(
  StorageFeeFactorIncrease -> Parameters.StorageFeeFactorDefault,
  MinValuePerByteIncrease -> Parameters.MinValuePerByteDefault,
  MaxBlockSizeIncrease -> 512 * 1024,
  MaxBlockCostIncrease -> 1000000,
  BlockVersion -> 1
))
