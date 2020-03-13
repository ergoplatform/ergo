package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.serialization.ErgoWalletSerializer
import scorex.util.serialization.{Reader, Writer}

import scala.util.{Failure, Try}

/**
  * HD key derivation path (see: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
  */
final case class DerivationPath(decodedPath: Seq[Int], publicBranch: Boolean) {

  import DerivationPath._

  def depth: Int = decodedPath.length

  def index: Long = decodedPath.last

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

  def toPublic: DerivationPath = this.copy(publicBranch = true)

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
