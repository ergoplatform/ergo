package org.ergoplatform.tools

object FeeSimulator extends App {
  import org.ergoplatform.settings.LaunchParameters._
  import org.ergoplatform.settings.Constants._

  //standard output size is about 80 bytes
  val StdSize = 80

  println("K: " + K )

  lazy val perOutputFee = StdSize * K  / CoinsInOneErgo.toDouble
  println("Storage fee for ordinary output: " + perOutputFee)

  val bitcoinUtxos = 60000000
  println("Reward per block: " + perOutputFee * (bitcoinUtxos / StoragePeriod))

  val minStdDust = MinValuePerByte * StdSize
  println(s"Min dust value of standard-size box: $minStdDust")
}
