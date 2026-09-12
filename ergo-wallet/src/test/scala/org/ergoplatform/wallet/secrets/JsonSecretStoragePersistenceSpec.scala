package org.ergoplatform.wallet.secrets

import org.ergoplatform.sdk.SecretString
import org.ergoplatform.sdk.wallet.settings.EncryptionSettings
import org.ergoplatform.wallet.settings.SecretStorageSettings
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import java.io.{File, IOException, Writer}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileAlreadyExistsException, Files, Path}
import java.nio.file.attribute.{PosixFileAttributeView, PosixFilePermissions}

class JsonSecretStoragePersistenceSpec extends AnyPropSpec with Matchers with FileUtils {
  private val encryption = EncryptionSettings("HmacSHA256", 1, 256)
  private val contents = """{"test":"checked persistence"}"""

  private def entries(dir: File): Set[String] = dir.listFiles().map(_.getName).toSet

  property("wallet discovery ignores an unpublished staging file") {
    val dir = createTempDir
    Files.createFile(dir.toPath.resolve(".ergo-secret-staging-example.tmp"))

    JsonSecretStorage.readFile(SecretStorageSettings(dir.getAbsolutePath, encryption)) shouldBe 'failure
  }

  property("initialization erases its seed when the secret directory cannot be created") {
    val parent = createTempDir
    val occupied = Files.createFile(parent.toPath.resolve("occupied"))
    val seed = Array.fill[Byte](32)(1)
    val settings = SecretStorageSettings(occupied.resolve("wallet").toString, encryption)

    intercept[java.io.IOException] {
      JsonSecretStorage.init(seed, SecretString.create("test password"), usePre1627KeyDerivation = false)(settings)
    }

    seed shouldBe Array.fill[Byte](32)(0)
  }

  property("initialization publishes one complete wallet and erases its seed") {
    val dir = createTempDir
    val seed = Array.fill[Byte](32)(1)
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)
    val storage = JsonSecretStorage.init(seed, SecretString.create("test password"), usePre1627KeyDerivation = false)(settings)

    entries(dir) shouldBe Set(storage.secretFile.getName)
    storage.secretFile.getName should endWith(".json")
    seed shouldBe Array.fill[Byte](32)(0)
    val reopened = JsonSecretStorage.readFile(settings).get
    reopened.unlock(SecretString.create("test password")) shouldBe 'success
    reopened.secret.get.usePre1627KeyDerivation shouldBe false
    reopened.lock()
  }

  property("wallet discovery preserves a sole legacy filename alongside staging files") {
    val dir = createTempDir
    val legacy = Files.createFile(dir.toPath.resolve("legacy-wallet"))
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)

    JsonSecretStorage.readFile(settings).get.secretFile.toPath shouldBe legacy
    Files.createFile(dir.toPath.resolve(".ergo-secret-staging-example.tmp"))
    JsonSecretStorage.readFile(settings).get.secretFile.toPath shouldBe legacy
  }

  property("wallet publication occurs only after the checked writer closes") {
    val dir = createTempDir
    val file = new File(dir, "wallet.json")
    var closed = false
    val openWriter: Path => Writer = path => {
      file.exists() shouldBe false
      val delegate = Files.newBufferedWriter(path, UTF_8)
      new Writer {
        override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
          file.exists() shouldBe false
          delegate.write(chars, offset, length)
        }
        override def flush(): Unit = delegate.flush()
        override def close(): Unit = {
          file.exists() shouldBe false
          delegate.close()
          closed = true
        }
      }
    }

    JsonSecretStorage.persist(file, contents, openWriter)

    closed shouldBe true
    new String(Files.readAllBytes(file.toPath), UTF_8) shouldBe contents
    entries(dir) shouldBe Set("wallet.json")
  }

  property("a writer open error propagates and removes only the owned staging file") {
    val dir = createTempDir
    val existing = Files.write(dir.toPath.resolve("existing.json"), contents.getBytes(UTF_8))
    val file = new File(dir, "wallet.json")
    val error = new IOException("controlled open failure")

    intercept[IOException] {
      JsonSecretStorage.persist(file, contents, _ => throw error)
    } shouldBe error

    entries(dir) shouldBe Set("existing.json")
    new String(Files.readAllBytes(existing), UTF_8) shouldBe contents
  }

  for ((failWrite, failClose) <- Seq((true, false), (false, true), (true, true))) {
    property(s"checked persistence propagates writer failures: write=$failWrite, close=$failClose") {
      val dir = createTempDir
      val file = new File(dir, "wallet.json")
      val writeError = new IOException("controlled write failure")
      val closeError = new IOException("controlled close failure")
      var closeCount = 0
      val openWriter: Path => Writer = path => {
        val delegate = Files.newBufferedWriter(path, UTF_8)
        new Writer {
          override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
            if (failWrite) throw writeError
            delegate.write(chars, offset, length)
          }
          override def flush(): Unit = delegate.flush()
          override def close(): Unit = {
            closeCount += 1
            delegate.close()
            if (failClose) throw closeError
          }
        }
      }

      val thrown = intercept[IOException] {
        JsonSecretStorage.persist(file, contents, openWriter)
      }

      thrown shouldBe (if (failWrite) writeError else closeError)
      thrown.getSuppressed.toSeq shouldBe (if (failWrite && failClose) Seq(closeError) else Seq.empty)
      closeCount shouldBe 1
      file.exists() shouldBe false
      entries(dir) shouldBe Set.empty
    }
  }

  property("low-level persistence refuses an already occupied exact destination") {
    val dir = createTempDir
    val file = Files.write(dir.toPath.resolve("wallet.json"), contents.getBytes(UTF_8)).toFile

    intercept[FileAlreadyExistsException] {
      JsonSecretStorage.persist(file, "replacement")
    }

    new String(Files.readAllBytes(file.toPath), UTF_8) shouldBe contents
    entries(dir) shouldBe Set("wallet.json")
  }

  property("a second initialization refuses an existing wallet and erases the new seed") {
    val dir = createTempDir
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)
    val first = JsonSecretStorage.init(Array.fill[Byte](32)(1), SecretString.create("first password"), false)(settings)
    val original = Files.readAllBytes(first.secretFile.toPath)
    val nextSeed = Array.fill[Byte](32)(2)

    intercept[FileAlreadyExistsException] {
      JsonSecretStorage.init(nextSeed, SecretString.create("second password"), false)(settings)
    }
    nextSeed shouldBe Array.fill[Byte](32)(0)
    entries(dir) shouldBe Set(first.secretFile.getName)
    Files.readAllBytes(first.secretFile.toPath) shouldBe original
    first.unlock(SecretString.create("first password")) shouldBe 'success
    first.lock()
  }

  property("initialization refuses a legacy filename and leaves it unchanged") {
    val dir = createTempDir
    val legacy = Files.write(dir.toPath.resolve("legacy-wallet"), contents.getBytes(UTF_8))
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)
    val seed = Array.fill[Byte](32)(1)
    intercept[FileAlreadyExistsException] {
      JsonSecretStorage.init(seed, SecretString.create("test password"), false)(settings)
    }
    Files.readAllBytes(legacy) shouldBe contents.getBytes(UTF_8)
    seed shouldBe Array.fill[Byte](32)(0)
    JsonSecretStorage.readFile(settings).get.secretFile.toPath shouldBe legacy
  }

  property("wallet discovery rejects ambiguous files instead of choosing directory order") {
    val dir = createTempDir
    Files.write(dir.toPath.resolve("first.json"), contents.getBytes(UTF_8))
    Files.write(dir.toPath.resolve("second.json"), contents.getBytes(UTF_8))
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)
    JsonSecretStorage.readFile(settings) shouldBe 'failure
    val seed = Array.fill[Byte](32)(1)
    intercept[FileAlreadyExistsException] {
      JsonSecretStorage.init(seed, SecretString.create("test password"), false)(settings)
    }
    seed shouldBe Array.fill[Byte](32)(0)
    entries(dir) shouldBe Set("first.json", "second.json")
  }

  property("initialization preserves unrelated staging material and explicitly restricts POSIX permissions") {
    val dir = createTempDir
    val staged = Files.write(dir.toPath.resolve(".ergo-secret-staging-other.tmp"), contents.getBytes(UTF_8))
    val inactive = Files.createDirectory(dir.toPath.resolve(".ergo-secret-staging-wallet-example"))
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)
    val storage = JsonSecretStorage.init(Array.fill[Byte](32)(1), SecretString.create("test password"), false)(settings)
    entries(dir) shouldBe Set(staged.getFileName.toString, inactive.getFileName.toString, storage.secretFile.getName)
    Files.readAllBytes(staged) shouldBe contents.getBytes(UTF_8)
    JsonSecretStorage.readFile(settings).get.secretFile shouldBe storage.secretFile
    if (Files.getFileAttributeView(storage.secretFile.toPath, classOf[PosixFileAttributeView]) != null) {
      Files.getPosixFilePermissions(storage.secretFile.toPath) shouldBe PosixFilePermissions.fromString("rw-------")
    }
  }

  property("wallet discovery rejects a legacy wallet alongside a JSON wallet") {
    val dir = createTempDir
    val legacy = Files.write(dir.toPath.resolve("legacy-wallet"), contents.getBytes(UTF_8))
    val json = Files.write(dir.toPath.resolve("current.json"), contents.getBytes(UTF_8))
    val settings = SecretStorageSettings(dir.getAbsolutePath, encryption)
    JsonSecretStorage.readFile(settings) shouldBe 'failure
    JsonSecretStorage.readFile(settings).failed.get should not be a[JsonSecretStorage.SecretFileNotFoundException]
    Files.readAllBytes(legacy) shouldBe contents.getBytes(UTF_8)
    Files.readAllBytes(json) shouldBe contents.getBytes(UTF_8)
  }

  property("wallet discovery reports a file used as the secret directory as an error") {
    val dir = createTempDir
    val occupied = Files.write(dir.toPath.resolve("occupied"), contents.getBytes(UTF_8))
    val settings = SecretStorageSettings(occupied.toString, encryption)
    JsonSecretStorage.readFile(settings) shouldBe 'failure
    JsonSecretStorage.readFile(settings).failed.get should not be a[JsonSecretStorage.SecretFileNotFoundException]
    Files.readAllBytes(occupied) shouldBe contents.getBytes(UTF_8)
  }

  property("wallet discovery distinguishes absence from invalid and ambiguous inventory") {
    val dir = createTempDir
    val settings = SecretStorageSettings(dir.toPath.resolve("missing").toString, encryption)
    JsonSecretStorage.readFile(settings).failed.get shouldBe a[JsonSecretStorage.SecretFileNotFoundException]
    Files.createDirectory(new File(settings.secretDir).toPath)
    JsonSecretStorage.readFile(settings).failed.get shouldBe a[JsonSecretStorage.SecretFileNotFoundException]
    Files.write(new File(settings.secretDir).toPath.resolve("one.json"), contents.getBytes(UTF_8))
    Files.write(new File(settings.secretDir).toPath.resolve("two.json"), contents.getBytes(UTF_8))
    JsonSecretStorage.readFile(settings).failed.get should not be a[JsonSecretStorage.SecretFileNotFoundException]
  }

  property("wallet discovery rejects a dangling directory symlink while preserving valid directory links") {
    val dir = createTempDir
    if (Files.getFileAttributeView(dir.toPath, classOf[PosixFileAttributeView]) != null) {
      val missing = dir.toPath.resolve("missing")
      val link = Files.createSymbolicLink(dir.toPath.resolve("keystore"), missing)
      val settings = SecretStorageSettings(link.toString, encryption)
      JsonSecretStorage.readFile(settings) shouldBe 'failure
      JsonSecretStorage.readFile(settings).failed.get should not be a[JsonSecretStorage.SecretFileNotFoundException]
      Files.createDirectory(missing)
      JsonSecretStorage.readFile(settings).failed.get shouldBe a[JsonSecretStorage.SecretFileNotFoundException]
      val wallet = Files.write(missing.resolve("wallet.json"), contents.getBytes(UTF_8))
      JsonSecretStorage.readFile(settings).get.secretFile.toPath.toRealPath() shouldBe wallet.toRealPath()
    }
  }

  property("a publication error propagates and removes the owned staging file") {
    val dir = createTempDir
    val file = new File(dir, "wallet.json")
    val openWriter: Path => Writer = path => {
      val delegate = Files.newBufferedWriter(path, UTF_8)
      new Writer {
        override def write(chars: Array[Char], offset: Int, length: Int): Unit = delegate.write(chars, offset, length)
        override def flush(): Unit = delegate.flush()
        override def close(): Unit = {
          delegate.close()
          // A non-empty destination directory deterministically rejects file publication.
          Files.createDirectory(file.toPath)
          Files.write(file.toPath.resolve("existing"), contents.getBytes(UTF_8))
        }
      }
    }

    intercept[IOException] {
      JsonSecretStorage.persist(file, contents, openWriter)
    }

    entries(dir) shouldBe Set("wallet.json")
    new String(Files.readAllBytes(file.toPath.resolve("existing")), UTF_8) shouldBe contents
  }
}
