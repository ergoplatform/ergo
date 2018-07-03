package org.ergoplatform.nodeView.state

import com.google.common.primitives.{Bytes, Ints}
import org.ergoplatform.modifiers.history.Header
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.crypto.authds.ADDigest

import scala.util.Try

/**
  * Additional data required for transactions validation
  *
  * @param height - height of the next block
  * @param digest - An AVL tree root hash of the UTXO state BEFORE current block application
  */
case class ErgoStateContext(height: Int, digest: ADDigest) extends BytesSerializable {

  override type M = ErgoStateContext

  override def serializer: Serializer[M] = ErgoStateContextSerializer

  def appendHeader(header: Header): ErgoStateContext = {
    ErgoStateContext(header.height + 1, header.stateRoot)
  }
}

object ErgoStateContextSerializer extends Serializer[ErgoStateContext] {

  override def toBytes(obj: ErgoStateContext): Array[Byte] = {
    Bytes.concat(obj.digest, Ints.toByteArray(obj.height))
  }

  override def parseBytes(bytes: Array[Byte]): Try[ErgoStateContext] = Try {
    val digest = ADDigest @@ bytes.take(33)
    val height = Ints.fromByteArray(bytes.slice(33, 37))
    ErgoStateContext(height, digest)
  }
}