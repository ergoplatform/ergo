package scorex.crypto.authds.avltree.batch

import scorex.crypto.hash.{Blake2b256, Digest32}


/**
  * Commonly used constants
  *
  * //todo: move to core module once it is established
  */
object Constants {
  /**
    * Type of hash function output
    */
  type DigestType = Digest32

  /**
    * Length of hash function output
    */
  val HashLength: Int = 32

  /**
    * Type of hash function used in the protocol
    */
  type HashFnType = Blake2b256.type

  /**
    * Thread-safe instance of hash function used in the protocol
    */
  val hashFn: HashFnType = Blake2b256
}
