package org.ergoplatform.nodeView.state

import com.google.common.primitives.Bytes
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.settings.Constants
import scapi.sigma.DLogProtocol.ProveDlog
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest

import scala.util.Try

/**
  * Additional data required for transactions validation
  *
  * @param lastHeaders - fixed number of last headers
  * @param genesisStateDigest - fixed number of last headers
  */
case class ErgoStateContext(lastHeaders: Seq[Header], genesisStateDigest: ADDigest)
  extends BytesSerializable with ScorexEncoding {

  // todo
  val lastBlockMinerPk: Array[Byte] = Array.fill(32)(0: Byte)

  // State root hash before the last block
  val previousStateDigest: ADDigest = if (lastHeaders.length >= 2) {
    lastHeaders(1).stateRoot
  } else {
    genesisStateDigest
  }

  def lastHeaderOpt: Option[Header] = lastHeaders.headOption

  // TODO it should be -1 by default, see https://github.com/ergoplatform/ergo/issues/546
  val currentHeight: Int = lastHeaderOpt.map(_.height).getOrElse(0)

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer

  def appendHeader(header: Header): ErgoStateContext = {
    ErgoStateContext(header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1), genesisStateDigest)
  }

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(previousStateDigest)}, $lastHeaders)"
}

object ErgoStateContext {

  def empty(genesisStateDigest: ADDigest): ErgoStateContext = {
    ErgoStateContext(Seq(), genesisStateDigest)
  }

  def apply(header: Header, genesisStateDigest: ADDigest): ErgoStateContext = {
    ErgoStateContext(Seq(header), genesisStateDigest)
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {

  override def toBytes(obj: ErgoStateContext): Array[Byte] = {
    Bytes.concat(obj.genesisStateDigest,
      scorex.core.utils.concatBytes(obj.lastHeaders.map(_.bytes)))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val genesisDigest = ADDigest @@ bytes.take(33)
    val length = bytes.length

    def loop(offset: Int, acc: Seq[Header]): Seq[Header] = if (offset < length) {
      // todo use only required bytes when header size will be fixed after https://github.com/ergoplatform/ergo/issues/452
      val header = HeaderSerializer.parseBytes(bytes.slice(offset, bytes.length)).get
      loop(offset + header.bytes.length, header +: acc)
    } else {
      acc.reverse
    }

    ErgoStateContext(loop(33, Seq()), genesisDigest)
  }
}