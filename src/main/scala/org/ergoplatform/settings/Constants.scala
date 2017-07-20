package org.ergoplatform.settings

import Algos.hashLength

object Constants {
  val MaxTarget = BigInt(1, Array.fill(hashLength)(Byte.MinValue))
  val InitialDifficulty = BigInt(1)
  val ModifierIdSize: Int = hashLength
}
