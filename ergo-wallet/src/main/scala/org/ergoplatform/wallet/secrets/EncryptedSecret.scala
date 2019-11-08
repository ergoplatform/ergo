package org.ergoplatform.wallet.secrets

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
            cipherParams: EncryptionSettings): EncryptedSecret = new EncryptedSecret(
    Base16.encode(cipherText), Base16.encode(salt), Base16.encode(iv), Base16.encode(authTag), cipherParams
  )
}
