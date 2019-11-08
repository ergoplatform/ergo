package org.ergoplatform.wallet.secrets

trait ExtendedKey {

  val keyBytes: Array[Byte]

  val chainCode: Array[Byte]

  val path: DerivationPath

  def child(idx: Int): ExtendedKey

  def derive(upPath: DerivationPath): ExtendedKey = {
    require(
      upPath.depth >= path.depth &&
        upPath.decodedPath.take(path.depth).zip(path.decodedPath).forall { case (i1, i2) => i1 == i2 } &&
        upPath.publicBranch == path.publicBranch,
      s"Incompatible paths: $upPath, $path"
    )
    upPath.decodedPath.drop(path.depth).foldLeft(this)(_ child _)
  }

}
