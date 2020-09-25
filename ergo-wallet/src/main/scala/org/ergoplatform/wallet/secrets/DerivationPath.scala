package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.Constants
import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}

import scala.util.{Failure, Success, Try}

/**
  * HD key derivation path (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final case class DerivationPath(decodedPath: Seq[Int], publicBranch: Boolean) {

  import DerivationPath._

  def depth: Int = decodedPath.length

  /**
    * @return last element of the derivation path, e.g. 2 for m/1/2
    */
  def index: Int = decodedPath.last

  def isMaster: Boolean = depth == 1

  /** Encode this DerivationPath as a parsable string. */
  def encoded: String = {
    val masterPrefix = if (publicBranch) s"$PublicBranchMasterId/" else s"$PrivateBranchMasterId/"
    val tailPath = decodedPath.tail
      .map(x => if (Index.isHardened(x)) s"${x - Index.HardRangeStart}'" else x.toString)
      .mkString("/")
    masterPrefix + tailPath
  }

  def extended(idx: Int): DerivationPath = DerivationPath(decodedPath :+ idx, publicBranch)

  /**
    * @return path with last element of the derivation path being increased, e.g. m/1/2 -> m/1/3
    */
  def increased: DerivationPath = DerivationPath(decodedPath.dropRight(1) :+ (index + 1), publicBranch)

  /**
    * Convert the derivation path to public branch. See BIP-32 for details.
    * @return derivation path from the public branch
    */
  def toPublicBranch: DerivationPath = this.copy(publicBranch = true)

  /**
    * Convert the derivation path to private branch. See BIP-32 for details.
    * @return derivation path from the private branch
    */
  def toPrivateBranch: DerivationPath = this.copy(publicBranch = false)

  override def toString: String = encoded

  def bytes: Array[Byte] = DerivationPathSerializer.toBytes(this)
}

object DerivationPath {

  val PublicBranchMasterId = "M"
  val PrivateBranchMasterId = "m"

  val MasterPath: DerivationPath = DerivationPath(List(0), publicBranch = false)

  def fromEncoded(path: String): Try[DerivationPath] = {
    val split = path.split("/")
    if (!split.headOption.exists(Seq(PublicBranchMasterId, PrivateBranchMasterId).contains)) {
      Failure(new Exception("Wrong path format"))
    } else {
      val pathTry = split.tail.foldLeft(Try(List(0))) { case (accTry, sym) =>
        accTry.flatMap { acc =>
          Try(if (sym.endsWith("'")) Index.hardIndex(sym.dropRight(1).toInt) else sym.toInt)
            .map(acc :+ _)
        }
      }
      val isPublicBranch = split.head == PublicBranchMasterId
      pathTry.map(DerivationPath(_, isPublicBranch))
    }
  }

  /**
    * Finds next available path index for a new key.
    * @secrets - secrets previously generated generated
    * @oldDerivation - whether to use pre-EIP3 derivation or not
    */
  def nextPath(secrets: IndexedSeq[ExtendedSecretKey],
               oldDerivation: Boolean = false): Try[DerivationPath] = {

    def pathSequence(secret: ExtendedSecretKey): Seq[Int] = secret.path.decodedPath.tail

    @scala.annotation.tailrec
    def nextPath(accPath: List[Int], remaining: Seq[Seq[Int]]): Try[DerivationPath] = {
      if (!remaining.forall(_.isEmpty)) {
        val maxChildIdx = remaining.flatMap(_.headOption).max
        if (!Index.isHardened(maxChildIdx)) {
          Success(DerivationPath(0 +: (accPath :+ maxChildIdx + 1), publicBranch = false))
        } else {
          nextPath(accPath :+ maxChildIdx, remaining.map(_.drop(1)))
        }
      } else {
        val exc = new Exception("Out of non-hardened index space. Try to derive hardened key specifying path manually")
        Failure(exc)
      }
    }

    if (secrets.size == 1) {
      // If pre-EIP generation, the first key generated after master's would be m/1, otherwise m/44'/429'/0'/0/0
      val path = if(oldDerivation) {
        Constants.oldDerivation
      } else {
        Constants.eip3DerivationPath
      }
      Success(path)
    } else {
      // If last secret corresponds to EIP-3 path, do EIP-3 derivation, otherwise, old derivation
      // For EIP-3 derivation, we increase last segment, m/44'/429'/0'/0/0 -> m/44'/429'/0'/0/1 and so on
      // For old derivation, we increase last non-hardened segment, m/1/1 -> m/2
      if (pathSequence(secrets.last).startsWith(Constants.eip3DerivationPath.decodedPath.tail.take(3))) {
        Success(secrets.last.path.increased)
      } else {
        nextPath(List.empty, secrets.map(pathSequence))
      }
    }
  }

}

object DerivationPathSerializer extends ErgoWalletSerializer[DerivationPath] {

  override def serialize(obj: DerivationPath, w: Writer): Unit = {
    w.put(if (obj.publicBranch) 0x01 else 0x00)
    w.putInt(obj.depth)
    obj.decodedPath.foreach(i => w.putBytes(Index.serializeIndex(i)))
  }

  override def parse(r: Reader): DerivationPath = {
    val publicBranch = if (r.getByte() == 0x01) true else false
    val depth = r.getInt()
    val path = (0 until depth).map(_ => Index.parseIndex(r.getBytes(4)))
    DerivationPath(path, publicBranch)
  }

}
