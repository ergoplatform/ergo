package org.ergoplatform.wallet.secrets

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import java.nio.file.{Files, Path}
import java.util

class ImportedSecretsStorageSpec
  extends AnyPropSpec
    with Matchers {

  private def tmpPath(): Path = {
    val dir = Files.createTempDirectory("imported-secrets-test")
    dir.toFile.deleteOnExit() // best-effort JVM-shutdown cleanup; only File exposes this hook
    ImportedSecretsStorage.pathAt(dir.toString)
  }

  private def randomScalar(): Array[Byte] = scorex.utils.Random.randomBytes(32)
  private def randomPubkey(): Array[Byte] = scorex.utils.Random.randomBytes(33)
  private def randomKek(): Array[Byte] = scorex.utils.Random.randomBytes(32)

  property("empty storage unlocks to an empty list") {
    val storage = new ImportedSecretsStorage(tmpPath())
    storage.unlock(randomKek()).get
    storage.isLocked shouldBe false
    storage.secrets shouldBe Some(IndexedSeq.empty)
    storage.publicKeys shouldBe IndexedSeq.empty
  }

  property("append round-trips through file + unlock") {
    val path = tmpPath()
    val kek = randomKek()
    val s1 = randomScalar()
    val s2 = randomScalar()
    val p1 = randomPubkey()
    val p2 = randomPubkey()

    val a = new ImportedSecretsStorage(path)
    a.unlock(kek).get
    a.append(s1, p1).get
    a.append(s2, p2).get
    a.secrets.get.length shouldBe 2
    util.Arrays.equals(a.secrets.get(0), s1) shouldBe true
    util.Arrays.equals(a.secrets.get(1), s2) shouldBe true

    val b = new ImportedSecretsStorage(path)
    b.unlock(kek).get
    b.secrets.get.length shouldBe 2
    util.Arrays.equals(b.secrets.get(0), s1) shouldBe true
    util.Arrays.equals(b.secrets.get(1), s2) shouldBe true
    util.Arrays.equals(b.publicKeys(0), p1) shouldBe true
    util.Arrays.equals(b.publicKeys(1), p2) shouldBe true
  }

  property("decrypt fails with wrong KEK") {
    val path = tmpPath()
    val storage = new ImportedSecretsStorage(path)
    storage.unlock(randomKek()).get
    storage.append(randomScalar(), randomPubkey()).get

    val other = new ImportedSecretsStorage(path)
    other.unlock(randomKek()).isFailure shouldBe true
  }

  property("lock zeroes plaintext and clears the in-memory list") {
    val storage = new ImportedSecretsStorage(tmpPath())
    val kek = randomKek()
    storage.unlock(kek).get
    val scalar = randomScalar()
    val pubkey = randomPubkey()
    storage.append(scalar, pubkey).get

    val handle = storage.secrets.get.head
    storage.lock()
    storage.isLocked shouldBe true
    storage.secrets shouldBe None
    handle.forall(_ == 0.toByte) shouldBe true
  }

  property("append before unlock is rejected") {
    val storage = new ImportedSecretsStorage(tmpPath())
    storage.append(randomScalar(), randomPubkey()).isFailure shouldBe true
  }

  property("KEK derivation is deterministic and depends on seed") {
    val seed = randomScalar()
    val k1 = ImportedSecretsStorage.deriveKek(seed)
    val k2 = ImportedSecretsStorage.deriveKek(seed)
    util.Arrays.equals(k1, k2) shouldBe true

    val differentSeed = randomScalar()
    val k3 = ImportedSecretsStorage.deriveKek(differentSeed)
    util.Arrays.equals(k1, k3) shouldBe false
    k1.length shouldBe 32
  }

}
