package org.ergoplatform.wallet.secrets

import io.circe.parser._
import io.circe.syntax._
import io.circe.{Encoder, Decoder, HCursor, Json}
import org.ergoplatform.wallet.settings.EncryptionSettings
import scorex.util.encode.Base16

/**
  * Describes structure of file storing encrypted seed.
  * @param cipherText   - encrypted seed
  * @param salt         - sequence of bits, known as a cryptographic salt
  * @param iv           - cipher initialization vector
  * @param authTag      - message authentication tag
  * @param cipherParams - cipher params
  */
final case class EncryptedSecret(cipherText: String, salt: String, iv: String, authTag: String,
                                 cipherParams: EncryptionSettings)

object EncryptedSecret {
  def apply(cipherText: Array[Byte], salt: Array[Byte], iv: Array[Byte], authTag: Array[Byte],
            cipherParams: EncryptionSettings): EncryptedSecret = {
    new EncryptedSecret(
      Base16.encode(cipherText),
      Base16.encode(salt),
      Base16.encode(iv),
      Base16.encode(authTag), cipherParams)
  }

  implicit object EncryptedSecretEncoder extends Encoder[EncryptedSecret] {

    def apply(secret: EncryptedSecret): Json = {
      Json.obj(
        "cipherText" -> secret.cipherText.asJson,
        "salt" -> secret.salt.asJson,
        "iv" -> secret.iv.asJson,
        "authTag" -> secret.authTag.asJson,
        "cipherParams" -> secret.cipherParams.asJson
      )
    }

  }

  implicit object EncryptedSecretDecoder extends Decoder[EncryptedSecret] {

    def apply(cursor: HCursor): Decoder.Result[EncryptedSecret] = {
      for {
        cipherText <- cursor.downField("cipherText").as[String]
        salt <- cursor.downField("salt").as[String]
        iv <- cursor.downField("iv").as[String]
        authTag <- cursor.downField("authTag").as[String]
        cipherParams <- cursor.downField("cipherParams").as[EncryptionSettings]
      } yield EncryptedSecret(cipherText, salt, iv, authTag, cipherParams)
    }

  }

//  def main(args: Array[String]): Unit = {
//    val res = decode[EncryptedSecret](
//      """{ "cipherText": "", "salt": "", "iv": "", "authTag": "", "cipherParams": { "prf":"abc", "c": 10, "dkLen": 100 }}""")
//    val jsonRaw = res.right.get.asJson.noSpaces
//    println(res.toString)
//    println(jsonRaw)
//  }

}
