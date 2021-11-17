package org.ergoplatform.wallet.utils

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._
import scala.util.Try

trait FileUtils {

  protected val randomPrefixLength = 10

  /**
    * Perform recursive deletion of directory content.
    */
  def deleteRecursive(root: File): Unit = {
    if (root.exists()) {
      Files.walk(root.toPath).iterator().asScala.toSeq.reverse.foreach(path => Try(Files.delete(path)))
    }
  }

  def createTempFile: java.io.File = {
    val dir = createTempDir
    val prefix = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    val suffix = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    val file = java.nio.file.Files.createTempFile(dir.toPath, prefix, suffix).toFile
    file.deleteOnExit()
    file
  }

  def createTempDir: java.io.File = {
    val rndString = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    createTempDirForPrefix(rndString)
  }

  private def createTempDirForPrefix(prefix: String): java.io.File = {
    val file = java.nio.file.Files.createTempDirectory(prefix).toFile
    file.deleteOnExit()
    file
  }
}
