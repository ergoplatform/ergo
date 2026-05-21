package org.ergoplatform

import org.ergoplatform.settings.{Args, NetworkType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

class ErgoAppSpec extends AnyFlatSpec with Matchers {

  "applyDefaults" should "default networkTypeOpt to MainNet when no network flag is given" in {
    val args = Args(userConfigPathOpt = Some("/some/explicit/path"), networkTypeOpt = None)
    ErgoApp.applyDefaults(args).networkTypeOpt shouldBe Some(NetworkType.MainNet)
  }

  it should "preserve an explicit --testnet flag" in {
    val args = Args(Some("/x"), Some(NetworkType.TestNet))
    ErgoApp.applyDefaults(args).networkTypeOpt shouldBe Some(NetworkType.TestNet)
  }

  it should "preserve an explicit --devnet flag" in {
    val args = Args(Some("/x"), Some(NetworkType.DevNet))
    ErgoApp.applyDefaults(args).networkTypeOpt shouldBe Some(NetworkType.DevNet)
  }

  it should "preserve an explicit --mainnet flag" in {
    val args = Args(Some("/x"), Some(NetworkType.MainNet))
    ErgoApp.applyDefaults(args).networkTypeOpt shouldBe Some(NetworkType.MainNet)
  }

  it should "preserve an explicit user config path" in {
    val args = Args(Some("/explicit/path.conf"), Some(NetworkType.MainNet))
    ErgoApp.applyDefaults(args).userConfigPathOpt shouldBe Some("/explicit/path.conf")
  }

  "defaultUserConfigPath" should "return the path when <baseDir>/.ergo/ergo.conf exists" in {
    val tmpHome = Files.createTempDirectory("ergo-app-spec-home")
    try {
      val ergoDir = Files.createDirectories(tmpHome.resolve(".ergo"))
      val confFile = Files.write(ergoDir.resolve(ErgoApp.DefaultConfigFileName), "ergo {}".getBytes)
      ErgoApp.defaultUserConfigPath(tmpHome.toAbsolutePath.toString) shouldBe
        Some(confFile.toAbsolutePath.toString)
    } finally {
      deleteRecursively(tmpHome)
    }
  }

  it should "return None when ergo.conf is absent" in {
    val tmpHome = Files.createTempDirectory("ergo-app-spec-empty")
    try {
      ErgoApp.defaultUserConfigPath(tmpHome.toAbsolutePath.toString) shouldBe None
    } finally {
      deleteRecursively(tmpHome)
    }
  }

  it should "return None for a null baseDir" in {
    ErgoApp.defaultUserConfigPath(null) shouldBe None
  }

  private def deleteRecursively(root: Path): Unit = {
    if (Files.exists(root)) {
      Files.walkFileTree(root, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }
        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
      ()
    }
  }
}
