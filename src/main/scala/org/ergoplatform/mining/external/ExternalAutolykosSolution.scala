package org.ergoplatform.mining.external

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.settings.Algos
import sigmastate.interpreter.CryptoConstants.EcPointType

/**
  * Solution of Autolykos PoW puzzle received from external miner.
  */
case class ExternalAutolykosSolution(w: EcPointType, n: Array[Byte], d: BigInt)

object ExternalAutolykosSolution extends ApiCodecs {

  implicit val jsonEncoder: Encoder[ExternalAutolykosSolution] = { s: ExternalAutolykosSolution =>
    Map(
      "w" -> s.w.asJson,
      "n" -> Algos.encode(s.n).asJson,
      "d" -> s.d.asJson(bigIntEncoder)
    ).asJson
  }

  implicit val jsonDecoder: Decoder[ExternalAutolykosSolution] = { c: HCursor =>
    for {
      w <- c.downField("w").as[EcPointType]
      n <- c.downField("n").as[Array[Byte]]
      d <- c.downField("d").as[BigInt]
    } yield ExternalAutolykosSolution(w, n, d)
  }

}
