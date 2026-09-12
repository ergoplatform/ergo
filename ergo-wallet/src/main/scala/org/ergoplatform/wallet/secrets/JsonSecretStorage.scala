package org.ergoplatform.wallet.secrets

import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.sdk.wallet.settings.EncryptionSettings
import org.ergoplatform.wallet.crypto
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.settings.SecretStorageSettings
import scorex.util.encode.Base16

import java.io.{File, FileNotFoundException, IOException, Writer}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileAlreadyExistsException, Files, LinkOption, NoSuchFileException, Path, StandardCopyOption}
import java.nio.file.attribute.{BasicFileAttributes, PosixFileAttributeView, PosixFilePermissions}
import java.util
import java.util.UUID
import scala.util.control.NonFatal
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

  /**
    * Tells if `secretsIndices` were locked and destroyed.
    */
  override def isLocked: Boolean = unlockedSecret.isEmpty

  /**
    * Returns the `secretsIndices` if already unlocked, or nothing.
    */
  override def secret: Option[ExtendedSecretKey] = unlockedSecret

  /**
    * @param mnemonic - SecretString mnemonic string to be erased after use.
    * @param mnemonicPassOpt - optional SecretString mnemonic password to be erased after use.
    */
  override def checkSeed(mnemonic: SecretString, mnemonicPassOpt: Option[SecretString]): Boolean = {
    unlockedSecret.fold(false){ uSecret => 
      val seed = Mnemonic.toSeed(mnemonic, mnemonicPassOpt)
      val secret = ExtendedSecretKey.deriveMasterKey(seed, uSecret.usePre1627KeyDerivation)
      secret.equals(uSecret)
    }
  }

  /**
    * Checks the seed can be decrypted, provided mnemonic with optional mnemonic password.
    * Makes secrets with `secretsIndices` available through `secrets` call.
    * @param pass - password to be used to decrypt secret, also SecretString to be erased after use
    */
  override def unlock(pass: SecretString): Try[Unit] = {
    val secretFileRaw = scala.io.Source.fromFile(secretFile, "UTF-8").getLines().mkString
    decode[EncryptedSecret](secretFileRaw)
      .right
      .map { encryptedSecret =>
        Base16.decode(encryptedSecret.cipherText)
          .flatMap(txt => Base16.decode(encryptedSecret.salt)
            .flatMap(salt => Base16.decode(encryptedSecret.iv)
              .flatMap(iv => Base16.decode(encryptedSecret.authTag)
                .map(tag => (txt, salt, iv, tag, encryptedSecret.usePre1627KeyDerivation))
              )
            )
          )
          .flatMap { case (cipherText, salt, iv, tag, usePre1627KeyDerivation) => {
              val res = crypto.AES.decrypt(cipherText, pass.getData(), salt, iv, tag)(encryptionSettings)
              res
                .map(seed => unlockedSecret = Some(ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation.getOrElse(true))))
            }
          }
      }
      .fold(Failure(_), Success(_))
      .flatten
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

  private val StagingPrefix = ".ergo-secret-staging-"

  /** Absence only; invalid or unreadable inventories use other failures. */
  final class SecretFileNotFoundException extends FileNotFoundException("Wallet secret file not found")

  private def storageEntries(settings: SecretStorageSettings): Seq[File] = {
    val dir = new File(settings.secretDir)
    val attributes = try {
      Some(Files.readAttributes(dir.toPath, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS))
    } catch {
      case _: NoSuchFileException => None
    }
    attributes match {
      case None => Seq.empty
      case Some(attrs) =>
        // An existing broken link is an invalid inventory, not an absent wallet directory.
        val isDirectory = if (attrs.isSymbolicLink) {
          Files.readAttributes(dir.toPath, classOf[BasicFileAttributes]).isDirectory
        } else attrs.isDirectory
        if (!isDirectory) throw new IOException("Wallet secret directory is not a directory")
        Option(dir.listFiles()).getOrElse(throw new IOException("Cannot list wallet secret directory"))
          .toSeq.filterNot(_.getName.startsWith(StagingPrefix))
    }
  }

  /**
   * Initializes storage instance with new wallet file encrypted with the given `pass`.
   * @param seed   - seed bytes, erased on both success and failure
   * @param pass   - encryption password
   * @param usePre1627KeyDerivation - use incorrect(previous) BIP32 derivation, expected to be false for new wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
   */
  def init(seed: Array[Byte], pass: SecretString, usePre1627KeyDerivation: Boolean)(settings: SecretStorageSettings): JsonSecretStorage = {
    try {
      // A fresh encryption produces a new filename; checking only that target cannot prevent reinitialization.
      if (storageEntries(settings).nonEmpty) {
        throw new FileAlreadyExistsException("Wallet secret directory is not empty")
      }
      val iv = scorex.utils.Random.randomBytes(crypto.AES.NonceBitsLen / 8)
      val salt = scorex.utils.Random.randomBytes(32)
      val (ciphertext, tag) = crypto.AES.encrypt(seed, pass.getData(), salt, iv)(settings.encryption)
      val encryptedSecret = EncryptedSecret(ciphertext, salt, iv, tag, settings.encryption, Some(usePre1627KeyDerivation))
      val uuid = UUID.nameUUIDFromBytes(ciphertext)
      val file = new File(s"${settings.secretDir}/$uuid.json")
      persist(file, encryptedSecret.asJson.noSpaces)
      new JsonSecretStorage(file, settings.encryption)
    } finally {
      util.Arrays.fill(seed, 0: Byte)
    }
  }

  /** Writes and closes a staging file before publishing it atomically. */
  private[secrets] def persist(file: File, jsonRaw: String): Unit =
    persist(file, jsonRaw, path => Files.newBufferedWriter(path, UTF_8))

  private[secrets] def persist(file: File, jsonRaw: String,
                               openWriter: Path => Writer): Unit = {
    val target = file.toPath.toAbsolutePath
    Files.createDirectories(target.getParent)
    if (Files.exists(target, LinkOption.NOFOLLOW_LINKS)) {
      throw new FileAlreadyExistsException(target.toString)
    }
    val staging = if (Files.getFileAttributeView(target.getParent, classOf[PosixFileAttributeView]) != null) {
      Files.createTempFile(target.getParent, StagingPrefix, ".tmp",
        PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")))
    } else {
      Files.createTempFile(target.getParent, StagingPrefix, ".tmp")
    }
    try {
      val writer = openWriter(staging)
      var writeFailure: Option[Throwable] = None
      try {
        writer.write(jsonRaw)
      } catch {
        case NonFatal(error) =>
          writeFailure = Some(error)
          throw error
      } finally {
        writeFailure match {
          case Some(error) => Try(writer.close()).failed.foreach(error.addSuppressed)
          case None => writer.close()
        }
      }
      // Do not expose a partial destination when atomic publication is unavailable.
      Files.move(staging, target, StandardCopyOption.ATOMIC_MOVE)
    } catch {
      case NonFatal(error) =>
        Try(Files.deleteIfExists(staging)).failed.foreach(error.addSuppressed)
        throw error
    }
  }

  /**
   * Initializes storage with the seed derived from an existing mnemonic phrase.
   * @param mnemonic - mnemonic phase
   * @param mnemonicPassOpt - optional mnemonic password
   * @param encryptionPass - encryption password
   * @param usePre1627KeyDerivation - use incorrect(previous) BIP32 derivation, expected to be false for new wallets, and true for old pre-1627 wallets (see https://github.com/ergoplatform/ergo/issues/1627 for details)
   */
  def restore(mnemonic: SecretString,
              mnemonicPassOpt: Option[SecretString],
              encryptionPass: SecretString,
              settings: SecretStorageSettings, 
              usePre1627KeyDerivation: Boolean): JsonSecretStorage = {
    val seed = Mnemonic.toSeed(mnemonic, mnemonicPassOpt)
    init(seed, encryptionPass, usePre1627KeyDerivation)(settings)
  }

  def readFile(settings: SecretStorageSettings): Try[JsonSecretStorage] = Try {
    storageEntries(settings) match {
      case Seq(file) if file.isFile => new JsonSecretStorage(file, settings.encryption)
      case Seq() => throw new SecretFileNotFoundException
      case _ => throw new IOException("Wallet secret directory does not contain one unambiguous wallet file")
    }
  }

}
