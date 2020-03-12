package org.ergoplatform.wallet.crypto

import javax.crypto.spec.{GCMParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKeyFactory}
import org.ergoplatform.wallet.settings.EncryptionSettings

import scala.util.Try

object AES {

  val AuthTagBitsLen = 128
  val NonceBitsLen = 96

  val CipherAlgo = "AES"
  val CipherAlgoInstance = s"$CipherAlgo/GCM/NoPadding"

  /**
    * @param data - data to encrypt
    * @param pass - password to derive encryption key from
    * @param salt - sequence of bits, known as a cryptographic salt
    * @param iv   - cipher initialization vector
    * @return     - tuple of resulted ciphertext and message auth tag
    */
  def encrypt(data: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte])
             (settings: EncryptionSettings): (Array[Byte], Array[Byte]) = {
    require(data.nonEmpty, "Empty data encryption attempt")
    val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
    val paramsSpec = new GCMParameterSpec(AuthTagBitsLen, iv)

    val cipher = Cipher.getInstance(CipherAlgoInstance)
    cipher.init(Cipher.ENCRYPT_MODE, keySpec, paramsSpec)

    val (authTag, ciphertext) = cipher.doFinal(data).splitAt(AuthTagBitsLen / 8)

    ciphertext -> authTag
  }

  /**
    * @param ciphertext - data to decrypt
    * @param pass       - password to derive decryption key from
    * @param salt       - sequence of bits, known as a cryptographic salt
    * @param iv         - cipher initialization vector
    * @param authTag    - message authentication tag
    */
  def decrypt(ciphertext: Array[Byte], pass: String, salt: Array[Byte], iv: Array[Byte], authTag: Array[Byte])
             (settings: EncryptionSettings): Try[Array[Byte]] = {
    require(ciphertext.nonEmpty, "Empty ciphertext decryption attempt")
    val keySpec = deriveEncryptionKeySpec(pass, salt)(settings)
    val paramsSpec = new GCMParameterSpec(AuthTagBitsLen, iv)

    val cipher = Cipher.getInstance(CipherAlgoInstance)
    cipher.init(Cipher.DECRYPT_MODE, keySpec, paramsSpec)

    Try(cipher.doFinal(authTag ++ ciphertext))
  }

  private def deriveEncryptionKeySpec(pass: String, salt: Array[Byte])
                                     (settings: EncryptionSettings): SecretKeySpec = {
    val pbeSpec = new PBEKeySpec(pass.toCharArray, salt, settings.c, settings.dkLen)
    val skf = SecretKeyFactory.getInstance(s"PBKDF2With${settings.prf}")
    val encryptionKey = skf.generateSecret(pbeSpec).getEncoded
    new SecretKeySpec(encryptionKey, CipherAlgo)
  }

}
