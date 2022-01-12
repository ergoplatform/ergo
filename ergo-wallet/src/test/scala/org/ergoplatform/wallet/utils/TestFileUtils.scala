package org.ergoplatform.wallet.utils

import java.nio.file.Path

trait TestFileUtils extends FileUtils {

  val basePath: Path = java.nio.file.Files.createTempDirectory(s"scorex-${System.nanoTime()}")

  sys.addShutdownHook {
    deleteRecursive(basePath.toFile)
  }
}
