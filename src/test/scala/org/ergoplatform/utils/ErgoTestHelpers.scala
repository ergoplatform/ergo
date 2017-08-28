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

  def with2Dirs(dirName1: String, dirName2: String)(action: (File, File) => Any): Unit = {
    val dir1 = new File(dirName1)
    val dir2 = new File(dirName2)
    Path(dir1).deleteRecursively()
    Path(dir2).deleteRecursively()
    dir1.mkdirs()
    dir2.mkdirs()
    action(dir1, dir2)
    Path(dir1).deleteRecursively()
    Path(dir2).deleteRecursively()
  }
}
