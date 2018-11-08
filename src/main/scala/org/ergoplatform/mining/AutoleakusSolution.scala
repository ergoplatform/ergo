package org.ergoplatform.mining

import com.google.common.primitives.Bytes
import io.circe.{Decoder, Encoder}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.api.ApiCodecs
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class AutoleakusSolution(pk: ECPoint, w: ECPoint, n: Array[Byte], d: BigInt) extends BytesSerializable {
  assert(!pk.isInfinity && !w.isInfinity, s"Infinity points are not allowed ${pk.isInfinity}, ${w.isInfinity}")

  override type M = AutoleakusSolution

  val encodedPk: Array[Byte] = pkToBytes(pk)

  override def serializer: Serializer[AutoleakusSolution] = AutoleakusSolutionSerializer
}

object AutoleakusSolution extends ApiCodecs {

  implicit val jsonEncoder: Encoder[AutoleakusSolution] = { s: AutoleakusSolution =>
    bytesEncoder.apply(s.bytes)
  }

  implicit val jsonDecoder: Decoder[AutoleakusSolution] = bytesDecoder.emapTry(AutoleakusSolutionSerializer.parseBytes)

}

object AutoleakusSolutionSerializer extends Serializer[AutoleakusSolution] {

  override def toBytes(obj: AutoleakusSolution): Array[Byte] = {
    Bytes.concat(pkToBytes(obj.pk), pkToBytes(obj.w), obj.n,
      BigIntegers.asUnsignedByteArray(obj.d.bigInteger))
  }

  override def parseBytes(bytes: Array[Byte]): Try[AutoleakusSolution] = Try {
    val pk = pkFromBytes(bytes.slice(0, PublicKeyLength))
    val pk2End = 2 * PublicKeyLength
    val w = pkFromBytes(bytes.slice(PublicKeyLength, pk2End))
    val nonce = bytes.slice(pk2End, pk2End + 8)
    val d = BigInt(BigIntegers.fromUnsignedByteArray(bytes.slice(pk2End + 8, bytes.length)))
    AutoleakusSolution(pk, w, nonce, d)
  }

}

