package scorex.crypto.authds.avltree.batch.helpers

import java.io.File

import scala.util.Random

trait FileHelper {

  def getRandomTempDir: File = {
    val dir = java.nio.file.Files.createTempDirectory("avliodb_test_" + Random.alphanumeric.take(15).mkString).toFile
    dir.deleteOnExit()
    dir
  }

}
