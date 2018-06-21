package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Shorts}
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.authds.ADDigest

import scala.util.Try

/**
  * Additional data, required for transactions validation.
  *
  * @param lastHeaders - fixed number of last applied header
  * @param digest      - AVL tree root hash state BEFORE current block application
  */
case class ErgoStateContext(lastHeaders: Seq[Header], digest: ADDigest) extends BytesSerializable {

  // Height of block we're trying to apply
  val height: Int = lastHeaders.lastOption.map(_.height + 1).getOrElse(0)

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer

  def appendHeader(header: Header): ErgoStateContext = {
    val newHeaders = if (lastHeaders.lengthCompare(Constants.LastHeadersAvailableForTxValidation) == 0) {
      lastHeaders.tail :+ header
    } else {
      lastHeaders :+ header
    }
    ErgoStateContext(newHeaders, header.stateRoot)
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {
  import scorex.core.utils.concatBytes

  override def toBytes(obj: ErgoStateContext): Array[Byte] = {
    val headersBytes = concatBytes(obj.lastHeaders.map(_.bytes)
      .map(b => Bytes.concat(Shorts.toByteArray(b.length.toShort), b)))
    Bytes.concat(obj.digest, headersBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val length =  bytes.length
    def parseHeaders(acc: Seq[Header], pos: Int): Seq[Header] = if(pos >= length) {
      acc
    } else {
      val bl = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
      val header = HeaderSerializer.parseBytes(bytes.slice(pos + 2, pos + 2 + bl)).get
      parseHeaders(acc :+ header, pos + 2 + bl)
    }
    val digest = ADDigest @@ bytes.take(33)
    val headers = parseHeaders(Seq(), 33)
    ErgoStateContext(headers, digest)
  }
}