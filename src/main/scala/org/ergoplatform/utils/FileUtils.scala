package org.ergoplatform.utils

import java.io.File

/**
 * Utilities to work with OS file system
 */
object FileUtils {
  /**
	* Perform recursive deletion of directory content.
	*/
  def deleteRecursive(dir: File): Unit = {
    for (file <- dir.listFiles) {
      if (!file.getName.startsWith(".")) {
        if (file.isDirectory) deleteRecursive(file)
        file.delete()
      }
    }
  }

}
