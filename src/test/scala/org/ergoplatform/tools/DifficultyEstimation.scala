package org.ergoplatform.tools

import org.ergoplatform.mining.difficulty.{DifficultyAdjustment, RequiredDifficulty}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.{Args, ErgoSettings, NetworkType}
import scorex.core.utils.NetworkTimeProvider

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object v2testing extends App {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val ergoSettings: ErgoSettings = ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/mainnet.conf"), Some(NetworkType.MainNet)))

  val ntp = new NetworkTimeProvider(ergoSettings.scorexSettings.ntp)

  val ldc = new DifficultyAdjustment(ergoSettings.chainSettings)

  val eh = ErgoHistory.readOrGenerate(ergoSettings, ntp)

  val epochLength = ergoSettings.chainSettings.epochLength

  println("best: " + eh.bestHeaderOpt.map(_.height))

  val heights = ldc.previousHeadersRequiredForRecalculation(843776 + 1 + 1024, epochLength)

  println("hs: " + heights)

  val headerOpts = heights.map(eh.bestHeaderIdAtHeight).map(idOpt => idOpt.flatMap(id => eh.typedModifierById[Header](id)))

  println(headerOpts.map(_.map(_.id)))

  println("dir: " + ergoSettings.directory)

  val h1 = headerOpts.head.get.copy(height = 843776 + 1024, nBits = 122447235L, timestamp = System.currentTimeMillis() + 1000*60*1440*6)

  val headers = headerOpts.flatten.toSeq ++ Seq(h1)

  val diff1 = ldc.calculate(headers, epochLength)
  println("diff1: " + diff1)
  val nbits1 = RequiredDifficulty.encodeCompactBits(diff1)

  val h2 = headerOpts.head.get.copy(height = 843776 + 2048, nBits = nbits1, timestamp = System.currentTimeMillis() + 1000*60*1440*10)

  val headers2 = headerOpts.flatten.toSeq.tail ++ Seq(h1, h2)
  val diff2 = ldc.calculate(headers2, epochLength)
  println("diff2: " + diff2)
  val nbits2 = RequiredDifficulty.encodeCompactBits(diff2)

  val h3 = headerOpts.head.get.copy(height = 843776 + 3072, nBits = nbits2, timestamp = System.currentTimeMillis() + (1000*60*1440*11).toInt)
  val headers3 = headers2.tail ++ Seq(h3)
  val diff3 = ldc.calculate(headers3, epochLength)
  println("diff3: " + diff3)
  val nbits3 = RequiredDifficulty.encodeCompactBits(diff3)

  val h4 = headerOpts.head.get.copy(height = 843776 + 4096, nBits = nbits3, timestamp = System.currentTimeMillis() + (1000*60*1440*11.25).toInt)
  val headers4 = headers3.tail ++ Seq(h4)
  val diff4 = ldc.calculate(headers4, epochLength)
  println("diff4: " + diff4)
  val nbits4 = RequiredDifficulty.encodeCompactBits(diff4)


  val h5 = headerOpts.head.get.copy(height = 843776 + 4096 + 1024, nBits = nbits3, timestamp = System.currentTimeMillis() + (1000*60*1440*13.25).toInt)
  val headers5 = headers4.tail ++ Seq(h5)
  val diff5 = ldc.calculate(headers5, epochLength)
  println("diff5: " + diff5)
  val nbits5 = RequiredDifficulty.encodeCompactBits(diff5)


  val h6 = headerOpts.head.get.copy(height = 843776 + 4096 + 2048, nBits = nbits3, timestamp = System.currentTimeMillis() + (1000*60*1440*16.5).toInt)
  val headers6 = headers5.tail ++ Seq(h6)
  val diff6 = ldc.calculate(headers6, epochLength)
  println("diff6: " + diff6)
  val nbits6 = RequiredDifficulty.encodeCompactBits(diff6)

}



object AltDiff extends App {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val currentSettings: ErgoSettings =
    ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/mainnet.conf"), Some(NetworkType.MainNet)))

  private val altSettings: ErgoSettings =
    ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/alt.conf"), Some(NetworkType.MainNet)))

  val ntp = new NetworkTimeProvider(altSettings.scorexSettings.ntp)

  val epochLength = altSettings.chainSettings.epochLength


  println(currentSettings.chainSettings.epochLength)
  println(altSettings.chainSettings.epochLength)

  val eh = ErgoHistory.readOrGenerate(altSettings, ntp)

  println("best: " + eh.bestHeaderOpt.map(_.height))

  val ldc = new DifficultyAdjustment(altSettings.chainSettings)

  (1 to 843207).foreach{h =>
    if(h % 1024 == 1 && h > 1) {
      val heights = ldc.previousHeadersRequiredForRecalculation(h, epochLength)
      val headers = heights.map(eh.bestHeaderIdAtHeight).map(idOpt => idOpt.flatMap(id => eh.typedModifierById[Header](id))).flatten
      val calcDiff = ldc.eip37Calculate(headers, epochLength)
      val chainDiff = eh.bestHeaderAtHeight(h).get.requiredDifficulty

      println(s"calculated diff for $h: $calcDiff, chain diff: $chainDiff , difference: ${calcDiff*100/chainDiff-100}%")
    }
  }

}


object AdaptiveSimulator extends App {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val altSettings: ErgoSettings =
    ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/alt.conf"), Some(NetworkType.MainNet)))
  val ntp = new NetworkTimeProvider(altSettings.scorexSettings.ntp)

  val eh = ErgoHistory.readOrGenerate(altSettings, ntp)

  val ldc = new DifficultyAdjustment(altSettings.chainSettings)


  val h1 = eh.bestHeaderAtHeight(1).get

  var totalError = 0
  var maxDelay = 0

  (1 to 100).foreach { _ =>
    var blockDelay = altSettings.chainSettings.blockInterval
    val epochLength = altSettings.chainSettings.epochLength
    var price = 1000000

    val medianChange = 1 // price going up 1% epoch on average
    val variance = 30 // with +-20%

    val blocks = mutable.Map[Int, Header]()
    blocks.put(0, h1.copy(height = 0)) // put genesis block
    blocks.put(1, h1) // put genesis block

    val precision = BigInt("10000000000000000")
    // t = d*c / p
    // c = t*p/d
    val c = blockDelay.toMillis * price * precision / h1.requiredDifficulty
    println("c: " + c)


    129.to(32 * 1024 + 1, 128).foreach { h =>
      println("================================")
      println("height: " + h)

      val newPrice = price +
        Random.nextInt(price * medianChange / 100)  +
        (if (Random.nextBoolean()) {
          Random.nextInt(price * variance / 100)
        } else {
          -Random.nextInt(price * variance / 100)
        })
      /*
        val epoch = (h - 1) / 128
        val newPrice = if(epoch%16 <8){
          price + Random.nextInt(price * variance / 100)
        } else {
          price - Random.nextInt(price * variance / 100)
        } */

      println("price: " + newPrice)

      val newBlockDelay = (blocks(h - 128).requiredDifficulty * c / precision / newPrice).toLong

      val blockBefore = h1.copy(height = h - 1, timestamp = blocks(h - 128).timestamp + 127 * newBlockDelay, nBits = blocks(h - 128).nBits)
      blocks.put(h - 1, blockBefore)

      val heights = ldc.previousHeadersRequiredForRecalculation(h, epochLength)
      val hs = heights.map(blocks.apply)

      val newDiff = ldc.bitcoinCalculate(hs, epochLength)
      println("newDiff: " + newDiff)
      val block = h1.copy(height = h, timestamp = blockBefore.timestamp + newBlockDelay, nBits = RequiredDifficulty.encodeCompactBits(newDiff))
      blocks.put(h, block)

      price = newPrice
      blockDelay = FiniteDuration(newBlockDelay, TimeUnit.MILLISECONDS)
      println("block delay: " + blockDelay.toSeconds + " s.")
      totalError += Math.abs(altSettings.chainSettings.blockInterval.toMillis - blockDelay.toMillis).toInt
      if (blockDelay.toSeconds > maxDelay) {
        maxDelay = blockDelay.toSeconds.toInt
      }
    }
  }

  println("Total error: " + totalError / 1000)
  println("Max delay: " + maxDelay)

}
