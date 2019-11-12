package org.ergoplatform.wallet.secrets

import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.ergoplatform.wallet.utils.{FileUtils, Generators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class JsonSecretStorageSpec
  extends PropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with Generators
    with FileUtils {

  property("initialization and unlock") {
    forAll(entropyGen, passwordGen, encryptionSettingsGen) { (seed, pass, cryptoSettings) =>
      val dir = createTempDir
      val settings = SecretStorageSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.init(seed, pass)(settings)

      storage.isLocked shouldBe true

      val unlockTry = storage.unlock(pass)

      unlockTry shouldBe 'success

      storage.isLocked shouldBe false
    }
  }

  property("secrets erasure on lock") {
    forAll(entropyGen, passwordGen, encryptionSettingsGen) { (seed, pass, cryptoSettings) =>
      val dir = createTempDir
      val settings = SecretStorageSettings(dir.getAbsolutePath, cryptoSettings)
      val storage = JsonSecretStorage.init(seed, pass)(settings)

      storage.unlock(pass)

      val secret = storage.secret

      secret.nonEmpty shouldBe true

      storage.lock()

      secret.forall(_.isErased) shouldBe true

      storage.secret.isEmpty shouldBe true
    }
  }

}
