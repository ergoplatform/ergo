package org.ergoplatform.settings


object Constants {
  val hashLength = 32
  val MaxTarget = BigInt(1, Array.fill(hashLength)((-1).toByte))
  val InitialDifficulty = BigInt(1)
  val ModifierIdSize: Int = hashLength
}
