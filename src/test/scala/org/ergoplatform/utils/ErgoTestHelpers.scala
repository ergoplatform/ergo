package org.ergoplatform.utils

import java.io.File

import scorex.testkit.TestkitHelpers

import scala.reflect.io.Path

trait ErgoTestHelpers extends TestkitHelpers {
  def withDir(dirName: String)(action: File => Any): Unit = {
    val dir = new File(dirName)
    dir.mkdirs()
    action(dir)
    Path.apply(dir).deleteRecursively()
  }
}
