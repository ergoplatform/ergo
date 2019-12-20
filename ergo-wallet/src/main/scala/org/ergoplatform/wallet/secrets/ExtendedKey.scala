package org.ergoplatform.wallet.secrets

/** Description from https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
  *
  * We extend both private and public keys first with an extra 256 bits of entropy.
  * This extension, called the chain code, is identical for corresponding private and public keys
  * and consists of 32 bytes.
  * We represent an extended private key as (k, c), with k the normal private key,
  * and c the chain code. An extended public key is represented as (K, c),
  * with K = point(k) and c the chain code.
  *
  * Each extended key has 2^31 normal child keys, and 2^31 hardened child keys.
  * Each of these child keys has an index. The normal child keys use indices 0 through 2^31-1.
  * The hardened child keys use indices 2^31 through `2^32-1`.
  */
trait ExtendedKey {

  val keyBytes: Array[Byte]

  val chainCode: Array[Byte]

  val path: DerivationPath

  /** Given a parent extended key and an index `idx`, it is possible to compute the corresponding
    * child extended key. The algorithm to do so depends on whether the child is a hardened key
    * or not (or, equivalently, whether `idx ≥ 2^31`), and whether we're talking about private or
    * public keys.
    * @see implementations in derived classes
    */
  def child(idx: Int): ExtendedKey

  def derive(upPath: DerivationPath): ExtendedKey = {
    require(
      upPath.depth >= path.depth &&
        upPath.decodedPath.take(path.depth).zip(path.decodedPath).forall { case (i1, i2) => i1 == i2 } &&
        upPath.publicBranch == path.publicBranch,
      s"Incompatible paths: $upPath, $path"
    )
    upPath.decodedPath.drop(path.depth).foldLeft(this)((parent, i) => parent.child(i))
  }

}
