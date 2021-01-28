package org.ergoplatform.mining

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.settings.Algos
import scorex.core.block.Block.Version
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

/**
  * Solution for an Autolykos PoW puzzle.
  *
  * In Autolykos v.1 all the four fields are used, in Autolykos v.2 only pk and n fields are used.
  *
  * @param pk - miner public key. Should be used to collect block rewards
  * @param w  - one-time public key. Prevents revealing of miners secret
  * @param n  - nonce (8 bytes)
  * @param d  - distance between pseudo-random number, corresponding to nonce `n` and a secret,
  *           corresponding to `pk`. The lower `d` is, the harder it was to find this solution.
  */
case class AutolykosSolution(pk: EcPointType,
                             w: EcPointType,
                             n: Array[Byte],
                             d: BigInt) {
  val encodedPk: Array[Byte] = groupElemToBytes(pk)
}

object AutolykosSolution extends ApiCodecs {
  // "pk", "w" and "d" values for Autolykos v2 solution, where they not passed from outside
  val pkForV2: EcPointType = CryptoConstants.dlogGroup.identity
  val wForV2: EcPointType = CryptoConstants.dlogGroup.generator
  val dForV2: BigInt = 0

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
      pkOpt <- c.downField("pk").as[Option[EcPointType]]
      wOpt <- c.downField("w").as[Option[EcPointType]]
      n <- c.downField("n").as[Array[Byte]]
      dOpt <- c.downField("d").as[Option[BigInt]]
    } yield {
      AutolykosSolution(pkOpt.getOrElse(pkForV2), wOpt.getOrElse(wForV2), n, dOpt.getOrElse(dForV2))
    }
  }

}


/**
  * Binary serializer for Autolykos v1 solution,
  * serializing and parsing "pk", "w", "nonce", and "d" values
  */
class AutolykosV1SolutionSerializer extends ScorexSerializer[AutolykosSolution] {

  override def serialize(obj: AutolykosSolution, w: Writer): Unit = {
    val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    w.putBytes(groupElemToBytes(obj.pk))
    w.putBytes(groupElemToBytes(obj.w))
    require(obj.n.length == 8) // non-consensus check on prover side
    w.putBytes(obj.n)
    w.putUByte(dBytes.length)
    w.putBytes(dBytes)
  }

  override def parse(r: Reader): AutolykosSolution = {
    val pk = groupElemFromBytes(r.getBytes(PublicKeyLength))
    val w = groupElemFromBytes(r.getBytes(PublicKeyLength))
    val nonce = r.getBytes(8)
    val dBytesLength = r.getUByte()
    val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
    AutolykosSolution(pk, w, nonce, d)
  }

}

/**
  * Binary serializer for Autolykos v2 solution, serializing and parsing "pk" and "nonce" values
  */
class AutolykosV2SolutionSerializer extends ScorexSerializer[AutolykosSolution] {

  import AutolykosSolution.{wForV2, dForV2}

  override def serialize(obj: AutolykosSolution, w: Writer): Unit = {
    w.putBytes(groupElemToBytes(obj.pk))
    require(obj.n.length == 8) // non-consensus check on prover side
    w.putBytes(obj.n)
  }

  override def parse(r: Reader): AutolykosSolution = {
    val pk = groupElemFromBytes(r.getBytes(PublicKeyLength))
    val nonce = r.getBytes(8)
    AutolykosSolution(pk, wForV2, nonce, dForV2)
  }

}


/**
  * Serializing facade for both Autolykos v1 and v2 solutions
  */
object AutolykosSolutionSerializer {
  private val v1Serializer = new AutolykosV1SolutionSerializer
  private val v2Serializer = new AutolykosV2SolutionSerializer

  private def serializer(blockVersion: Version): ScorexSerializer[AutolykosSolution] = {
    if (blockVersion == 1) {
      v1Serializer
    } else {
      v2Serializer
    }
  }

  def serialize(blockVersion: Version, solution: AutolykosSolution, w: Writer): Unit = {
    serializer(blockVersion).serialize(solution, w)
  }

  def parse(r: Reader, blockVersion: Version): AutolykosSolution = {
    serializer(blockVersion).parse(r)
  }

}
