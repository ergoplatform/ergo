package org.ergoplatform.mining

import com.google.common.primitives.Bytes
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.settings.Algos
import scorex.core.serialization.{BytesSerializable, Serializer}
import sigmastate.interpreter.CryptoConstants.EcPointType

import scala.util.Try

/**
  * Solution of Autolykos PoW puzzle
  *
  * @param pk - miner public key. Should be used to collect block rewards
  * @param w  - one-time public key. Prevents revealing of miners secret
  * @param n  - nonce
  * @param d  - distance between pseudo-random number, corresponding to nonce `n` and a secret,
  *           corresponding to `pk`. The lower `d` is, the harder it was to find this solution.
  */
case class AutolykosSolution(pk: EcPointType, w: EcPointType, n: Array[Byte], d: BigInt) extends BytesSerializable {
  override type M = AutolykosSolution

  val encodedPk: Array[Byte] = groupElemToBytes(pk)

  override def serializer: Serializer[AutolykosSolution] = AutolykosSolutionSerializer
}

object AutolykosSolution extends ApiCodecs {

  implicit val jsonEncoder: Encoder[AutolykosSolution] = { s: AutolykosSolution =>
    Map(
      "pk" -> s.pk.asJson,
      "w" -> s.w.asJson,
      "n" -> Algos.encode(s.n).asJson,
      "d" -> s.d.asJson(bigIntEncoder)
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AutolykosSolution] = { c: HCursor =>
    for {
      pk <- c.downField("pk").as[EcPointType]
      w <- c.downField("w").as[EcPointType]
      n <- c.downField("n").as[Array[Byte]]
      d <- c.downField("d").as[BigInt]
    } yield AutolykosSolution(pk: EcPointType, w: EcPointType, n: Array[Byte], d: BigInt)
  }
}

object AutolykosSolutionSerializer extends Serializer[AutolykosSolution] {

  override def toBytes(obj: AutolykosSolution): Array[Byte] = {
    val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    Bytes.concat(groupElemToBytes(obj.pk), groupElemToBytes(obj.w), obj.n, Array(dBytes.length.toByte), dBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[AutolykosSolution] = Try {
    val pk = groupElemFromBytes(bytes.slice(0, PublicKeyLength))
    val pk2End = 2 * PublicKeyLength
    val w = groupElemFromBytes(bytes.slice(PublicKeyLength, pk2End))
    val nonce = bytes.slice(pk2End, pk2End + 8)
    val dBytesLength = bytes(pk2End + 8)
    val d = BigInt(BigIntegers.fromUnsignedByteArray(bytes.slice(pk2End + 9, pk2End + 9 + dBytesLength)))
    AutolykosSolution(pk, w, nonce, d)
  }

}

