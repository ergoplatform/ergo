package org.ergoplatform.settings

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class NetworkConfigTempDirSpec extends AnyPropSpec with Matchers with BeforeAndAfterAll with FileUtils {

  // A user config outside the temp directory under test, so that directory starts empty
  private val home: Path = Files.createTempDirectory("network-conf-home")

  private val userConfig: Path = {
    Files.createDirectories(home.resolve("data"))
    Files.write(home.resolve("user.conf"), s"""ergo.directory = "${home.resolve("data")}"\n""".getBytes(UTF_8))
  }

  override def afterAll(): Unit = deleteRecursive(home.toFile)

  private def read(networkType: NetworkType): ErgoSettings =
    ErgoSettingsReader.read(Args(Some(userConfig.toString), Some(networkType)))

  private def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory("network-conf-tmp")
    val saved = System.getProperty("java.io.tmpdir")
    System.setProperty("java.io.tmpdir", dir.toString)
    try f(dir) finally {
      System.setProperty("java.io.tmpdir", saved)
      deleteRecursive(dir.toFile)
    }
  }

  property("reading a network config leaves nothing in java.io.tmpdir") {
    withTempDir { dir =>
      read(NetworkType.DevNet).networkType shouldBe NetworkType.DevNet
      dir.toFile.list().toSeq shouldBe empty
    }
  }

  property("a file already at the network config's name in java.io.tmpdir neither blocks nor changes reading") {
    withTempDir { dir =>
      val existing = dir.resolve("devnet.conf")
      val content = "ergo.networkType = \"testnet\"\n"
      Files.write(existing, content.getBytes(UTF_8))
      existing.toFile.setWritable(false) shouldBe true
      read(NetworkType.DevNet).networkType shouldBe NetworkType.DevNet
      new String(Files.readAllBytes(existing), UTF_8) shouldBe content
    }
  }

  property("two networks read in one process each keep their own network type") {
    withTempDir { _ =>
      read(NetworkType.DevNet).networkType shouldBe NetworkType.DevNet
      read(NetworkType.TestNet).networkType shouldBe NetworkType.TestNet
    }
  }
}
