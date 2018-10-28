package org.ergoplatform.mining

import com.google.common.primitives.Bytes
import io.circe.{Decoder, Encoder}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.autoleakus.group
import org.ergoplatform.autoleakus.pow.ksum.hashBinding.{HKSumNonce, HKSumSolution}
import scorex.core.serialization.{BytesSerializable, Serializer}

import scala.util.Try

case class AutoleakusSolution(pk: ECPoint, w: ECPoint, n: HKSumNonce, d: BigInt) extends BytesSerializable {

  override type M = AutoleakusSolution

  val encodedPk: Array[Byte] = pk.getEncoded(true)

  def solution(m: Array[Byte]): HKSumSolution = HKSumSolution(m, pk, w, n, d)

  override def serializer: Serializer[AutoleakusSolution] = AutoleakusSolutionSerializer
}

object AutoleakusSolution extends ApiCodecs {
  def apply(s: HKSumSolution): AutoleakusSolution = AutoleakusSolution(s.pk, s.w, s.n, s.d)

  implicit val jsonEncoder: Encoder[AutoleakusSolution] = { s: AutoleakusSolution =>
    bytesEncoder.apply(s.bytes)
  }

  implicit val jsonDecoder: Decoder[AutoleakusSolution] = bytesDecoder.emapTry(AutoleakusSolutionSerializer.parseBytes)

}

object AutoleakusSolutionSerializer extends Serializer[AutoleakusSolution] {

  override def toBytes(obj: AutoleakusSolution): Array[Byte] = {
    Bytes.concat(obj.pk.getEncoded(true), obj.w.getEncoded(true), obj.n.nonceBytes,
      BigIntegers.asUnsignedByteArray(obj.d.bigInteger))
  }

  override def parseBytes(bytes: Array[Byte]): Try[AutoleakusSolution] = Try {
    val pk = group.curve.decodePoint(bytes.slice(0, 33))
    val w = group.curve.decodePoint(bytes.slice(33, 66))
    val nonce = HKSumNonce(bytes.slice(66, 74))
    val d = BigInt(BigIntegers.fromUnsignedByteArray(bytes.slice(74, bytes.length)))
    AutoleakusSolution(pk, w, nonce, d)
  }

}

