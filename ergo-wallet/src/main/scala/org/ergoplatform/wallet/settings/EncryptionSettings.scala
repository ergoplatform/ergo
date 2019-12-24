package org.ergoplatform.wallet.settings

import io.circe.{Json, Encoder, Decoder, HCursor}
import io.circe.syntax._
import cats.syntax.either._  // don't remove, it is needed for scala 2.11

/**
  * Encryption parameters
  * @param prf   - pseudo-random function with output of length `dkLen` (PBKDF2 param)
  * @param c     - number of PBKDF2 iterations (PBKDF2 param)
  * @param dkLen - desired bit-length of the derived key (PBKDF2 param)
  */
final case class EncryptionSettings(prf: String, c: Int, dkLen: Int)

object EncryptionSettings {

  implicit object EncryptionSettingsEncoder extends Encoder[EncryptionSettings] {

    def apply(s: EncryptionSettings): Json = {
      Json.obj(
        "prf" -> s.prf.asJson,
        "c" -> s.c.asJson,
        "dkLen" -> s.dkLen.asJson
      )
    }

  }

  implicit object EncryptionSettingsDecoder extends Decoder[EncryptionSettings] {

    def apply(cursor: HCursor): Decoder.Result[EncryptionSettings] = {
      for {
        prf <- cursor.downField("prf").as[String]
        c <- cursor.downField("c").as[Int]
        dkLen <- cursor.downField("dkLen").as[Int]
      } yield EncryptionSettings(prf, c, dkLen)
    }

  }
}