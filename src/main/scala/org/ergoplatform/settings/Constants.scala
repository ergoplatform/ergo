package org.ergoplatform.settings

import scorex.crypto.hash.Blake2b256

object Constants {

  val hash = Blake2b256
  val MaxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
  val InitialDifficulty = BigInt(1)

}
