package org.ergoplatform.utils

import java.io.File

import scorex.testkit.TestkitHelpers

import scala.reflect.io.Path

trait ErgoTestHelpers extends TestkitHelpers {
  def withDir(dirName: String)(action: File => Any): Unit = {
    val dir = new File(dirName)
    Path(dir).deleteRecursively()
    dir.mkdirs()
    action(dir)
    Path(dir).deleteRecursively()
  }
}
