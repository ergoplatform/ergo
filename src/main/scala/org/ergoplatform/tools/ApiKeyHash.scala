package org.ergoplatform.tools

import org.ergoplatform.settings.Algos
import scorex.crypto.hash.Blake2b256

import scala.io.Source

object ApiKeyHash {

  def hash(apiKey: String): String = Algos.encoder.encode(Blake2b256(apiKey))

  def main(args: Array[String]): Unit = {
    val input = Source.fromInputStream(System.in, "UTF-8")
    try {
      val apiKey = input.mkString
      require(apiKey.nonEmpty, "API key must not be empty")
      println(hash(apiKey))
    } finally {
      input.close()
    }
  }
}
