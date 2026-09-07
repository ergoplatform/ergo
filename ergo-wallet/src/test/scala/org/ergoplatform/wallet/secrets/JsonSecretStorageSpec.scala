package org.ergoplatform.wallet.secrets

import io.circe.parser.decode
import io.circe.syntax._
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.sdk.wallet.settings.EncryptionSettings
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.ergoplatform.wallet.utils.FileUtils
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.UUID

class JsonSecretStorageSpec
  extends AnyPropSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with FileUtils {
  import org.ergoplatform.wallet.utils.WalletGenerators._

  property("initialization and unlock") {
    forAll(entropyGen, passwordGen, encryptionSettingsGen, Arbitrary.arbBool.arbitrary) { 
      (seed, pass, cryptoSettings, usePre1627KeyDerivation) =>
      val dir = createTempDir
      val settings = SecretStorageSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.init(seed, SecretString.create(pass), usePre1627KeyDerivation)(settings)

      storage.isLocked shouldBe true

      val unlockTry = storage.unlock(SecretString.create(pass))

      unlockTry shouldBe 'success

      storage.isLocked shouldBe false

      // wallet should use explicitly specified BIP32 key derivation
      storage.secret.get.usePre1627KeyDerivation shouldBe usePre1627KeyDerivation
    }
  }

  private val originalEncryption = EncryptionSettings("HmacSHA1", 7, 256)
  private val changedEncryptionSettings = Seq(
    "prf" -> originalEncryption.copy(prf = "HmacSHA256"),
    "c" -> originalEncryption.copy(c = 11),
    "dkLen" -> originalEncryption.copy(dkLen = 128)
  )

  changedEncryptionSettings.foreach { case (field, currentEncryption) =>
    property(s"reopening uses stored $field after configuration changes") {
      Seq(false, true).foreach { usePre1627KeyDerivation =>
        val dir = createTempDir
        try {
          val seed = Array.tabulate[Byte](32)(_.toByte)
          val expected = ExtendedSecretKey.deriveMasterKey(seed, usePre1627KeyDerivation)
          val pass = "synthetic-wallet-password"
          val settings = SecretStorageSettings(dir.getAbsolutePath, originalEncryption)
          val created = JsonSecretStorage.init(seed, SecretString.create(pass), usePre1627KeyDerivation)(settings)
          val originalBytes = Files.readAllBytes(created.secretFile.toPath)
          val reopened = JsonSecretStorage.readFile(settings.copy(encryption = currentEncryption)).get

          reopened.unlock(SecretString.create(pass)) shouldBe 'success
          reopened.secret.get shouldBe expected
          reopened.secret.get.usePre1627KeyDerivation shouldBe usePre1627KeyDerivation
          Files.readAllBytes(created.secretFile.toPath).toSeq shouldBe originalBytes.toSeq
          reopened.lock()
        } finally deleteRecursive(dir)
      }
    }

    property(s"initialization persists the current $field setting") {
      val dir = createTempDir
      try {
        val seed = Array.tabulate[Byte](32)(_.toByte)
        val pass = "synthetic-wallet-password"
        val settings = SecretStorageSettings(dir.getAbsolutePath, currentEncryption)
        val created = JsonSecretStorage.init(seed, SecretString.create(pass), usePre1627KeyDerivation = false)(settings)
        val raw = new String(Files.readAllBytes(created.secretFile.toPath), StandardCharsets.UTF_8)

        decode[EncryptedSecret](raw).right.get.cipherParams shouldBe currentEncryption
        created.unlock(SecretString.create(pass)) shouldBe 'success
        created.lock()
      } finally deleteRecursive(dir)
    }
  }

  Seq(
    "unsupported PRF" -> originalEncryption.copy(prf = "Unsupported"),
    "zero iterations" -> originalEncryption.copy(c = 0),
    "negative iterations" -> originalEncryption.copy(c = -1),
    "zero key length" -> originalEncryption.copy(dkLen = 0),
    "negative key length" -> originalEncryption.copy(dkLen = -1),
    "unsupported AES key length" -> originalEncryption.copy(dkLen = 120)
  ).foreach { case (description, invalidEncryption) =>
    property(s"invalid stored parameters return failure: $description") {
      val dir = createTempDir
      try {
        val pass = "synthetic-wallet-password"
        val settings = SecretStorageSettings(dir.getAbsolutePath, originalEncryption)
        val created = JsonSecretStorage.init(Array.fill[Byte](32)(1), SecretString.create(pass),
          usePre1627KeyDerivation = false)(settings)
        val raw = new String(Files.readAllBytes(created.secretFile.toPath), StandardCharsets.UTF_8)
        val encrypted = decode[EncryptedSecret](raw).right.get
        Files.write(created.secretFile.toPath,
          encrypted.copy(cipherParams = invalidEncryption).asJson.noSpaces.getBytes(StandardCharsets.UTF_8))
        val reopened = JsonSecretStorage.readFile(settings).get

        reopened.unlock(SecretString.create(pass)) shouldBe 'failure
        reopened.isLocked shouldBe true
        reopened.secret shouldBe None
      } finally deleteRecursive(dir)
    }
  }

  property("secrets erasure on lock") {
    forAll(entropyGen, passwordGen, encryptionSettingsGen) { (seed, pass, cryptoSettings) =>
      val dir = createTempDir
      val settings = SecretStorageSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.init(seed, SecretString.create(pass), usePre1627KeyDerivation = false)(settings)

      storage.unlock(SecretString.create(pass))

      val secret = storage.secret

      secret.nonEmpty shouldBe true

      storage.lock()

      secret.forall(_.isErased) shouldBe true

      storage.secret.isEmpty shouldBe true
    }
  }

  property("restore from mnemonic") {
    forAll(mnemonicGen, passwordGen, encryptionSettingsGen, Arbitrary.arbBool.arbitrary) { 
      (mnemonic, pass, cryptoSettings, usePre1627KeyDerivation) =>
      val dir = createTempDir
      val settings = SecretStorageSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.restore(SecretString.create(mnemonic.toString()), 
        mnemonicPassOpt = None,  
        encryptionPass = SecretString.create(pass), 
        settings = settings,
        usePre1627KeyDerivation)

      val _ = storage.unlock(SecretString.create(pass))

      // wallet should use explicitly specified BIP32 key derivation
      storage.secret.get.usePre1627KeyDerivation shouldBe usePre1627KeyDerivation
    }
  }

  property("pre 1627 key derivation is used for loaded storage with missing usePre1627KeyDerivation") {
    val dir = createTempDir
    // mock JSON file without usePre1627KeyDerivation property set, simulating pre 1627 wallet file
    val jsonFileRaw = """
    {"cipherText":"e134f488c52a87ccb0287fdd164bdb3b67f04c33ba94eab169e802df7a082addd434e1f36dccbf5362f97a9e57ef97879807bdc632072fb2b3ae9a9b08a6caf6","salt":"988e9d31c675bf6012c235e4c238f22649285140544b17600e2887f655b74ae7","iv":"dff8c6b120cdfaac4192e9c1","authTag":"de1c6443808263b749f4c29f146208ba","cipherParams":{"prf":"HmacSHA1","c":7,"dkLen":256}}
    """
    val pass = List("N", "m").toString()
    val encryptionSettings = EncryptionSettings("HmacSHA256", 11, 128)

    new File(dir.getAbsolutePath()).mkdirs()
    val uuid = UUID.randomUUID()
    val file = new File(s"${dir}/$uuid.json")
    val outWriter = new PrintWriter(file)
    outWriter.write(jsonFileRaw)
    outWriter.close()

    val settings = SecretStorageSettings(dir.getAbsolutePath, encryptionSettings)
    val storage = JsonSecretStorage.readFile(settings).get

    storage.unlock(SecretString.create(pass)) shouldBe 'success

    // loaded wallet should use pre 1627 key derivation
    storage.secret.get.usePre1627KeyDerivation shouldBe true
  }

}
