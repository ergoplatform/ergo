package org.ergoplatform.wallet.secrets

import java.io.{File, PrintWriter}
import java.util
import java.util.UUID

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.wallet.crypto
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.settings.{EncryptionSettings, SecretStorageSettings}
import scorex.util.encode.Base16

import scala.util.{Failure, Success, Try}

/**
  * Secret storage backend.
  * Stores encrypted seed in json file (structure is described by [[EncryptedSecret]]).
  * Responsible for managing access to the secrets.
  * (detailed storage specification: https://github.com/ergoplatform/ergo-wallet/wiki/Ergo-Secret-Storage)
  */
final class JsonSecretStorage(val secretFile: File, encryptionSettings: EncryptionSettings)
  extends SecretStorage {

  private var unlockedSecret: Option[ExtendedSecretKey] = None

  override def isLocked: Boolean = unlockedSecret.isEmpty

  override def secret: Option[ExtendedSecretKey] = unlockedSecret

  override def checkSeed(mnemonic: String, mnemonicPassOpt: Option[String]): Boolean = {
    val seed = Mnemonic.toSeed(mnemonic, mnemonicPassOpt)
    val secret = ExtendedSecretKey.deriveMasterKey(seed)
    unlockedSecret.fold(false)(s => secret.equals(s))
  }

  /**
    * Makes secrets with `secretsIndices` available through `secrets` call.
    * @param pass - password to be used to decrypt secret
    */
  override def unlock(pass: String): Try[Unit] = {
    val secretFileRaw = scala.io.Source.fromFile(secretFile, "UTF-8").getLines().mkString
    decode[EncryptedSecret](secretFileRaw)
      .right
      .map { encryptedSecret =>
        Base16.decode(encryptedSecret.cipherText)
          .flatMap(txt => Base16.decode(encryptedSecret.salt)
            .flatMap(salt => Base16.decode(encryptedSecret.iv)
              .flatMap(iv => Base16.decode(encryptedSecret.authTag)
                .map(tag => (txt, salt, iv, tag))
              )
            )
          )
          .flatMap { case (cipherText, salt, iv, tag) =>
            crypto.AES.decrypt(cipherText, pass, salt, iv, tag)(encryptionSettings)
          }
      }
      .fold(Failure(_), Success(_))
      .flatten
      .map(seed => unlockedSecret = Some(ExtendedSecretKey.deriveMasterKey(seed)))
  }

  /**
    * Destroys all loaded secrets.
    */
  override def lock(): Unit = {
    unlockedSecret.foreach(_.zeroSecret())
    unlockedSecret = None
  }

}

object JsonSecretStorage {

  /**
    * Initializes storage instance with new wallet file encrypted with the given `pass`.
    */
  def init(seed: Array[Byte], pass: String)(settings: SecretStorageSettings): JsonSecretStorage = {
    val iv = scorex.utils.Random.randomBytes(crypto.AES.NonceBitsLen / 8)
    val salt = scorex.utils.Random.randomBytes(32)
    val (ciphertext, tag) = crypto.AES.encrypt(seed, pass, salt, iv)(settings.encryption)
    val encryptedSecret = EncryptedSecret(ciphertext, salt, iv, tag, settings.encryption)
    val uuid = UUID.nameUUIDFromBytes(ciphertext)
    new File(settings.secretDir).mkdirs()
    val file = new File(s"${settings.secretDir}/$uuid.json")
    val outWriter = new PrintWriter(file)
    val jsonRaw = encryptedSecret.asJson.noSpaces

    outWriter.write(jsonRaw)
    outWriter.close()

    util.Arrays.fill(seed, 0: Byte)

    new JsonSecretStorage(file, settings.encryption)
  }

  /**
    * Initializes storage with the seed derived from an existing mnemonic phrase.
    */
  def restore(mnemonic: String, mnemonicPassOpt: Option[String], encryptionPass: String)
             (settings: SecretStorageSettings): JsonSecretStorage = {
    val seed = Mnemonic.toSeed(mnemonic, mnemonicPassOpt)
    init(seed, encryptionPass)(settings)
  }

}
