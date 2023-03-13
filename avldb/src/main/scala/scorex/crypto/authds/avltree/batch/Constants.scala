package scorex.crypto.authds.avltree.batch

import scorex.crypto.hash.{Blake2b256, Digest32}

object Constants {
  type HashFnType = Blake2b256.type
  type DigestType = Digest32

  val hashFn: HashFnType = Blake2b256
  val HashLength: Int = 32
}
