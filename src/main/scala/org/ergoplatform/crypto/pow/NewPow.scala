package org.ergoplatform.crypto.pow

class NewPow(secrets: Seq[(Array[Byte], Array[Byte])],
             n: Byte,
             k: Byte,
             p: BigInt,
             g: BigInt,
             h: Byte) {


}

object NewPow {

  /**
    * Hash that returns a number from [0,p)
    */
  def hash(message: Array[Byte], p: BigInt): BigInt = {
    ???
  }

  def apply(seed: Array[Byte],
            size: Int,
            n: Byte,
            k: Byte,
            p: BigInt,
            g: BigInt,
            h: Byte): NewPow = {
    ???
  }
}