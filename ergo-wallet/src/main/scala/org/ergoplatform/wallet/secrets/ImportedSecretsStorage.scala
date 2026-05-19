package org.ergoplatform.wallet.secrets

import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.sdk.wallet.secrets.{DerivationPath, ExtendedPublicKey, Index}
import scorex.util.encode.Base16
import sigma.crypto.{BigIntegers, CryptoFacade}
import sigmastate.crypto.DLogProtocol.DLogProverInput

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import javax.crypto.{AEADBadTagException, Cipher, Mac}
import scala.util.{Failure, Try}

/**
  * Persistent storage for raw (non-HD) private keys imported via WIF.
  *
  * Each scalar is encrypted with AES-256-GCM under a key-encryption key (KEK)
  * derived from the wallet's master seed. As a consequence imported keys are
  * available exactly when the master is unlocked: there is no separate password
  * prompt at import or export time.
  *
  * On disk: a single JSON file (`imported/keys.json` inside the wallet's secret
  * directory) holding a JSON array of [[EncryptedImportedKey]] records. The
  * file is rewritten atomically on each append. Absence of the file is
  * equivalent to "no imported keys".
  */
final class ImportedSecretsStorage(val path: Path) {

  private val GcmTagBits: Int = 128

  // KEK retained while wallet is unlocked. Zeroed on lock().
  private var unlockedKek: Option[Array[Byte]] = None
  // Decrypted scalars in the order they appear in the file. None == locked.
  private var unlockedSecrets: Option[Vector[Array[Byte]]] = None
  // Cached encrypted records (so append can rewrite the file without a re-read).
  private var encryptedRecords: Vector[EncryptedImportedKey] = Vector.empty

  def isLocked: Boolean = unlockedKek.isEmpty

  def secrets: Option[IndexedSeq[Array[Byte]]] = unlockedSecrets

  /**
    * Decrypt every record using the given KEK and populate the in-memory list.
    * If the file does not exist yet the storage unlocks with an empty list.
    */
  def unlock(kek: Array[Byte]): Try[Unit] = Try {
    val (records, decrypted) = if (Files.exists(path)) {
      val raw = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
      decode[Vector[EncryptedImportedKey]](raw) match {
        case Right(rs) =>
          val ds = rs.map(r => decryptRecord(r, kek).get)
          (rs, ds)
        case Left(err) =>
          throw new Exception(s"Failed to parse imported secrets file: $err")
      }
    } else {
      (Vector.empty[EncryptedImportedKey], Vector.empty[Array[Byte]])
    }
    encryptedRecords = records
    unlockedSecrets = Some(decrypted)
    unlockedKek = Some(kek.clone())
  }

  /**
    * Encrypt a new scalar and persist it. Requires the storage to be unlocked.
    * `publicKey` is stored alongside in plaintext so the lookup-on-export path
    * does not have to decrypt every record to find one by address.
    */
  def append(scalar: Array[Byte], publicKey: Array[Byte]): Try[Unit] = Try {
    require(scalar.length == Wif.SecretLength, "imported scalar must be 32 bytes")
    val kek = unlockedKek.getOrElse(
      throw new IllegalStateException("imported-secrets storage is locked")
    )
    val salt = scorex.utils.Random.randomBytes(32)
    val iv = scorex.utils.Random.randomBytes(12)
    val (cipherText, authTag) = encryptRaw(scalar, kek, iv)
    val record = EncryptedImportedKey(cipherText, salt, iv, authTag, publicKey)
    encryptedRecords = encryptedRecords :+ record
    unlockedSecrets = Some(unlockedSecrets.getOrElse(Vector.empty) :+ scalar.clone())
    writeFile()
  }

  /** Zero the plaintext list and the KEK. Encrypted-on-disk records are untouched. */
  def lock(): Unit = {
    unlockedSecrets.foreach(_.foreach(util.Arrays.fill(_, 0: Byte)))
    unlockedSecrets = None
    unlockedKek.foreach(util.Arrays.fill(_, 0: Byte))
    unlockedKek = None
  }

  /** Public-keys (compressed sec1 encoding) of all imported records, decrypted or not. */
  def publicKeys: IndexedSeq[Array[Byte]] =
    encryptedRecords.map(r => Base16.decode(r.publicKey).get)

  private def writeFile(): Unit = {
    Files.createDirectories(path.getParent)
    val tmp = path.resolveSibling(path.getFileName.toString + ".tmp")
    Files.write(tmp, encryptedRecords.asJson.noSpaces.getBytes(StandardCharsets.UTF_8))
    Files.move(
      tmp, path,
      StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING
    )
  }

  private def decryptRecord(r: EncryptedImportedKey, kek: Array[Byte]): Try[Array[Byte]] =
    for {
      ct  <- Base16.decode(r.cipherText)
      iv  <- Base16.decode(r.iv)
      tag <- Base16.decode(r.authTag)
      pt  <- decryptRaw(ct, kek, iv, tag)
    } yield pt

  private def encryptRaw(
    data: Array[Byte],
    key: Array[Byte],
    iv: Array[Byte]
  ): (Array[Byte], Array[Byte]) = {
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(
      Cipher.ENCRYPT_MODE,
      new SecretKeySpec(key, "AES"),
      new GCMParameterSpec(GcmTagBits, iv)
    )
    val out = cipher.doFinal(data)
    val tagLen = GcmTagBits / 8
    val cipherText = util.Arrays.copyOfRange(out, 0, out.length - tagLen)
    val tag = util.Arrays.copyOfRange(out, out.length - tagLen, out.length)
    (cipherText, tag)
  }

  private def decryptRaw(
    cipherText: Array[Byte],
    key: Array[Byte],
    iv: Array[Byte],
    authTag: Array[Byte]
  ): Try[Array[Byte]] = {
    val cipher = Cipher.getInstance("AES/GCM/NoPadding")
    cipher.init(
      Cipher.DECRYPT_MODE,
      new SecretKeySpec(key, "AES"),
      new GCMParameterSpec(GcmTagBits, iv)
    )
    Try(cipher.doFinal(cipherText ++ authTag)).recoverWith {
      case _: AEADBadTagException =>
        Failure(new Throwable("Imported-secret decryption failed (wrong key or tampered file)"))
      case e: Throwable =>
        Failure(e)
    }
  }

}

object ImportedSecretsStorage {

  /**
    * Sentinel BIP-32 path component used to mark imported (non-HD) public keys
    * inside the wallet's tracked-key cache. Picked far out of the practical
    * derivation space so it cannot collide with anything a user might derive.
    */
  val ImportedKeyPathTag: Int = Index.hardIndex(0x7FFFFFFE)

  /** Path used to identify imported key `idx`: `m/2147483646'/idx`. */
  def importedPathAt(idx: Int): DerivationPath =
    DerivationPath(List(0, ImportedKeyPathTag, idx), publicBranch = false)

  /** True iff the given path was created by `importedPathAt`. */
  def isImportedPath(path: DerivationPath): Boolean =
    path.decodedPath.length == 3 && path.decodedPath(1) == ImportedKeyPathTag

  /**
    * Build a fake `ExtendedPublicKey` for a non-HD secret. The chain code is
    * filled with zeroes since imported keys are never further derived from.
    */
  def fakeExtendedPubKey(scalar: Array[Byte], idx: Int): ExtendedPublicKey = {
    val input = DLogProverInput(BigIntegers.fromUnsignedByteArray(scalar))
    val pkBytes = CryptoFacade.getASN1Encoding(input.publicImage.value, true)
    new ExtendedPublicKey(pkBytes, Array.fill(32)(0: Byte), importedPathAt(idx))
  }

  /**
    * Default location: a subdirectory of the master-seed directory. The
    * subdirectory keeps `imported/keys.json` out of `JsonSecretStorage.readFile`'s
    * top-level `*.json` lookup so the master-seed picker is unaffected.
    */
  def pathAt(secretDir: String): Path = Paths.get(secretDir, "imported", "keys.json")

  /**
    * Derive a 32-byte KEK from the unlocked master seed bytes. Uses HMAC-SHA256
    * with a fixed label, so changing the label deliberately invalidates older
    * stored imported keys (future migration knob).
    */
  def deriveKek(masterSeed: Array[Byte]): Array[Byte] = {
    val label = "ergo-wif-import-v1".getBytes(StandardCharsets.UTF_8)
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(masterSeed, "HmacSHA256"))
    mac.doFinal(label)
  }

}
