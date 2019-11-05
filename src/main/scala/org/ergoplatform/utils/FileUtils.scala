package org.ergoplatform.utils

import java.io.File

object FileUtils {

  def deleteRecursive(dir: File): Unit = {
    for (file <- dir.listFiles) {
      if (!file.getName.startsWith(".")) {
        if (file.isDirectory) deleteRecursive(file)
        file.delete()
      }
    }
  }

}
