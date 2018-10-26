package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest

import scala.util.Try

/**
  * Additional data required for transactions validation
  *
  * @param currentHeight - height of the next block
  * @param stateDigest   - An AVL tree root hash of the UTXO state BEFORE current block application
  * @param lastHeaders   - fixed number of last headers
  */
case class ErgoStateContext(currentHeight: Int, stateDigest: ADDigest, lastHeaders: Seq[Header])
  extends BytesSerializable with ScorexEncoding {

  // todo remove currentHeight: Int, stateDigest: ADDigest,

  lazy val lastHeader: Header = lastHeaders.head

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer

  def appendHeader(header: Header): ErgoStateContext = {
    ErgoStateContext(header.height,
      header.stateRoot,
      header +: lastHeaders.takeRight(Constants.LastHeadersInContext - 1))
  }

  override def toString: String = s"ErgoStateContext($currentHeight,${encoder.encode(stateDigest)}, $lastHeaders)"
}

object ErgoStateContext {

  def empty(afterGenesisStateDigest: ADDigest):ErgoStateContext = {
    ErgoStateContext(0, afterGenesisStateDigest, Seq())
  }

  def apply(header: Header): ErgoStateContext = {
    ErgoStateContext(header.height, header.stateRoot, Seq(header))
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {

  override def toBytes(obj: ErgoStateContext): Array[Byte] = {
    Bytes.concat(obj.stateDigest,
      Ints.toByteArray(obj.currentHeight),
      scorex.core.utils.concatBytes(obj.lastHeaders.map(_.bytes)))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val digest = ADDigest @@ bytes.take(33)
    val height = Ints.fromByteArray(bytes.slice(33, 37))
    val length = bytes.length

    def loop(offset: Int, acc: Seq[Header]): Seq[Header] = if (offset < length) {
      // todo use only required bytes when header size will be fixed after https://github.com/ergoplatform/ergo/issues/452
      val header = HeaderSerializer.parseBytes(bytes.slice(offset, bytes.length)).get
      loop(offset + header.bytes.length, header +: acc)
    } else {
      acc.reverse
    }

    ErgoStateContext(height, digest, loop(37, Seq()))
  }
}
