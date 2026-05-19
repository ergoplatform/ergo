package org.ergoplatform.wallet.secrets

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import scorex.util.encode.Base16

/**
  * One AES-GCM-encrypted 32-byte secp256k1 scalar persisted in the
  * `imported/keys.json` file inside the wallet's secret directory.
  */
final case class EncryptedImportedKey(
  cipherText: String,
  salt: String,
  iv: String,
  authTag: String,
  publicKey: String
)

object EncryptedImportedKey {

  def apply(
    cipherText: Array[Byte],
    salt: Array[Byte],
    iv: Array[Byte],
    authTag: Array[Byte],
    publicKey: Array[Byte]
  ): EncryptedImportedKey =
    new EncryptedImportedKey(
      Base16.encode(cipherText),
      Base16.encode(salt),
      Base16.encode(iv),
      Base16.encode(authTag),
      Base16.encode(publicKey)
    )

  implicit val encoder: Encoder[EncryptedImportedKey] = deriveEncoder
  implicit val decoder: Decoder[EncryptedImportedKey] = deriveDecoder

}
