package org.ergoplatform.mining

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}

/**
  * Solution of Autolykos PoW puzzle
  *
  * @param pk - miner public key. Should be used to collect block rewards
  * @param w  - one-time public key. Prevents revealing of miners secret
  * @param n  - nonce
  * @param d  - distance between pseudo-random number, corresponding to nonce `n` and a secret,
  *           corresponding to `pk`. The lower `d` is, the harder it was to find this solution.
  */
case class AutolykosSolution(pk: ECPoint, w: ECPoint, n: Array[Byte], d: BigInt) {
  assert(!pk.isInfinity && !w.isInfinity, s"Infinity points are not allowed ${pk.isInfinity}, ${w.isInfinity}")

  val encodedPk: Array[Byte] = pkToBytes(pk)

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
      pk <- c.downField("pk").as[ECPoint]
      w <- c.downField("w").as[ECPoint]
      n <- c.downField("n").as[Array[Byte]]
      d <- c.downField("d").as[BigInt]
    } yield AutolykosSolution(pk: ECPoint, w: ECPoint, n: Array[Byte], d: BigInt)
  }
}

object AutolykosSolutionSerializer extends ScorexSerializer[AutolykosSolution] {


  override def serialize(obj: AutolykosSolution, w: Writer): Unit = {
    val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    w.putBytes(pkToBytes(obj.pk))
    w.putBytes(pkToBytes(obj.w))
    w.putBytes(obj.n)
    w.putUByte(dBytes.length)
    w.putBytes(dBytes)
  }

  override def parse(r: Reader): AutolykosSolution = {
    val pk = pkFromBytes(r.getBytes(PublicKeyLength))
    val w = pkFromBytes(r.getBytes(PublicKeyLength))
    val nonce = r.getBytes(8)
    val dBytesLength = r.getUByte()
    val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
    AutolykosSolution(pk, w, nonce, d)
  }
}

