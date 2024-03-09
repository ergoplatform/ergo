package org.ergoplatform.wallet.crypto

import org.ergoplatform.wallet.crypto
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class EncryptionSpec
  extends AnyPropSpec
    with Matchers {
  import org.ergoplatform.wallet.utils.WalletGenerators._

  property("AES encryption/decryption") {
    (0 to 100).foreach { _ =>
      (dataGen.sample, passwordGen.sample, encryptionSettingsGen.sample) match {
        case (Some(data), Some(pass), Some(settings)) =>
          val iv = scorex.utils.Random.randomBytes(16)
          val salt = scorex.utils.Random.randomBytes(32)
          val (encrypted, mac) = crypto.AES.encrypt(data, pass.toCharArray(), salt, iv)(settings)
          val decryptedTry = crypto.AES.decrypt(encrypted, pass.toCharArray(), salt, iv, mac)(settings)

          decryptedTry shouldBe 'success
          decryptedTry.get shouldEqual data
        case _ =>
      }
    }
  }

  property("AES encryption/decryption - failure on corrupted data decryption") {
    (0 to 100).foreach { _ =>
      (dataGen.sample, passwordGen.sample, encryptionSettingsGen.sample) match {
        case (Some(data), Some(pass), Some(settings)) =>
          val iv = scorex.utils.Random.randomBytes(16)
          val salt = scorex.utils.Random.randomBytes(32)
          val (encrypted, mac) = crypto.AES.encrypt(data, pass.toCharArray(), salt, iv)(settings)
          val modifiedBytes = encrypted.clone
          val idx = scala.util.Random.nextInt(encrypted.length)
          modifiedBytes.update(idx, ((modifiedBytes(idx) + 1) % Byte.MaxValue).toByte)
          val decryptedTry = crypto.AES.decrypt(modifiedBytes, pass.toCharArray(), salt, iv, mac)(settings)

          decryptedTry shouldBe 'failure
        case _ =>
      }
    }
  }
}
