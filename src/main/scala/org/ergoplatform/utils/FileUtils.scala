package org.ergoplatform.utils

import java.io.File
import java.nio.file.Files
import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Utilities to work with OS file system
  */
object FileUtils {

  /**
    * Perform recursive deletion of directory content.
    */
  def deleteRecursive(root: File): Unit = {
    if (root.exists()) {
      Files.walk(root.toPath).iterator().asScala.toSeq.reverse.foreach(path => Try(Files.delete(path)))
    }
  }

}
