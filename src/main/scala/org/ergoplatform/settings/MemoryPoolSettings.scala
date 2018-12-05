package org.ergoplatform.settings

/**
  * @param minTxByteFee minimum fee per byte value to store transaction in the pool
  *                     (and so include it into a block and broadcast it)
  * @param minCostUnitFee fee per computational complexity unit
  */
case class MemoryPoolSettings(maxPoolSize: Int, minTxByteFee: Long, minCostUnitFee: Long)

object MemoryPoolSettings {
  val default = MemoryPoolSettings(maxPoolSize = -1, minTxByteFee = 100, minCostUnitFee = 100)
}
