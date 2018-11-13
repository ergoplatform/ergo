package org.ergoplatform.mining

import com.google.common.primitives.Bytes
import io.circe.{Decoder, Encoder}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.api.ApiCodecs
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class AutolykosSolution(pk: ECPoint, w: ECPoint, n: Array[Byte], d: BigInt) extends BytesSerializable {
  assert(!pk.isInfinity && !w.isInfinity, s"Infinity points are not allowed ${pk.isInfinity}, ${w.isInfinity}")

  override type M = AutolykosSolution

  val encodedPk: Array[Byte] = pkToBytes(pk)

  override def serializer: Serializer[AutolykosSolution] = AutolykosSolutionSerializer
}

object AutolykosSolution extends ApiCodecs {

  implicit val jsonEncoder: Encoder[AutolykosSolution] = { s: AutolykosSolution =>
    bytesEncoder.apply(s.bytes)
  }

  implicit val jsonDecoder: Decoder[AutolykosSolution] = bytesDecoder.emapTry(AutolykosSolutionSerializer.parseBytes)

}

object AutolykosSolutionSerializer extends Serializer[AutolykosSolution] {

  override def toBytes(obj: AutolykosSolution): Array[Byte] = {
    val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    Bytes.concat(pkToBytes(obj.pk), pkToBytes(obj.w), obj.n, Array(dBytes.length.toByte), dBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AutolykosSolution] = Try {
    val pk = pkFromBytes(bytes.slice(0, PublicKeyLength))
    val pk2End = 2 * PublicKeyLength
    val w = pkFromBytes(bytes.slice(PublicKeyLength, pk2End))
    val nonce = bytes.slice(pk2End, pk2End + 8)
    val dBytesLength = bytes(pk2End + 8)
    val d = BigInt(BigIntegers.fromUnsignedByteArray(bytes.slice(pk2End + 9, pk2End + 9 + dBytesLength)))
    AutolykosSolution(pk, w, nonce, d)
  }

}

