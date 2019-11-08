package org.ergoplatform.wallet

import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16

package object secrets {

  def secretFromSeed(idx: Int, seed: String): Array[Byte] = Blake2b256.hash(idx + seed)

  def secretFromSeed(idx: Int, seed: Array[Byte]): Array[Byte] = Blake2b256.hash(idx + Base16.encode(seed))

}
