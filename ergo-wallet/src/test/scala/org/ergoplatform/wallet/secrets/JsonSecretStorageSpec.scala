package org.ergoplatform.wallet.secrets

import org.ergoplatform.sdk.wallet.settings.EncryptionSettings
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.ergoplatform.wallet.utils.{FileUtils, Generators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.ergoplatform.wallet.interface4j.SecretString
import org.scalacheck.Arbitrary

import java.io.{File, PrintWriter}
import java.util.UUID

class JsonSecretStorageSpec
  extends AnyPropSpec
    with Matchers
    with ScalaCheckPropertyChecks
    with Generators
    with FileUtils {

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
    val encryptionSettings = EncryptionSettings("HmacSHA1", 7, 256)

    new File(dir.getAbsolutePath()).mkdirs()
    val uuid = UUID.randomUUID()
    val file = new File(s"${dir}/$uuid.json")
    val outWriter = new PrintWriter(file)
    outWriter.write(jsonFileRaw)
    outWriter.close()

    val settings = SecretStorageSettings(dir.getAbsolutePath, encryptionSettings)
    val storage = JsonSecretStorage.readFile(settings).get

    val _ = storage.unlock(SecretString.create(pass))

    // loaded wallet should use pre 1627 key derivation
    storage.secret.get.usePre1627KeyDerivation shouldBe true
  }

}
