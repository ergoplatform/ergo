package org.ergoplatform.wallet.secrets

import io.circe.syntax._
import cats.syntax.either._  // don't remove, it is needed for scala 2.11
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.sdk.wallet.settings.EncryptionSettings
import scorex.util.encode.Base16

/**
  * Describes structure of file storing encrypted seed.
  * @param cipherText   - encrypted seed
  * @param salt         - sequence of bits, known as a cryptographic salt
  * @param iv           - cipher initialization vector
  * @param authTag      - message authentication tag
  * @param cipherParams - cipher params
  * @param usePre1627KeyDerivation - use incorrect(previous) BIP32 derivation, expected to be false for new wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
  */
final case class EncryptedSecret(cipherText: String, salt: String, iv: String, authTag: String,
                                 cipherParams: EncryptionSettings, usePre1627KeyDerivation: Option[Boolean])

object EncryptedSecret {
  def apply(cipherText: Array[Byte], salt: Array[Byte], iv: Array[Byte], authTag: Array[Byte],
            cipherParams: EncryptionSettings, usePre1627KeyDerivation: Option[Boolean]): EncryptedSecret = {
    new EncryptedSecret(
      Base16.encode(cipherText),
      Base16.encode(salt),
      Base16.encode(iv),
      Base16.encode(authTag), 
      cipherParams, 
      usePre1627KeyDerivation)
  }

  implicit object EncryptedSecretEncoder extends Encoder[EncryptedSecret] {

    def apply(secret: EncryptedSecret): Json = {
      Json.obj(
        "cipherText" -> secret.cipherText.asJson,
        "salt" -> secret.salt.asJson,
        "iv" -> secret.iv.asJson,
        "authTag" -> secret.authTag.asJson,
        "cipherParams" -> secret.cipherParams.asJson,
        "usePre1627KeyDerivation" -> secret.usePre1627KeyDerivation.asJson
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
        usePre1627KeyDerivation <- cursor.downField("usePre1627KeyDerivation").as[Option[Boolean]]
      } yield EncryptedSecret(cipherText, salt, iv, authTag, cipherParams, usePre1627KeyDerivation)
    }

  }

}
