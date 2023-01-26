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

  val eh = ErgoHistory.readOrGenerate(ergoSettings, ntp)(null)

  val epochLength = ergoSettings.chainSettings.epochLength

  println("Chain settings: " + ergoSettings.chainSettings)
  println("Epoch length: " + epochLength)
  println("Block time: " + ergoSettings.chainSettings.blockInterval)

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

  val eh = ErgoHistory.readOrGenerate(altSettings, ntp)(null)

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
  import io.circe.parser._

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val altSettings: ErgoSettings =
    ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/alt.conf"), Some(NetworkType.MainNet)))
  val ntp = new NetworkTimeProvider(altSettings.scorexSettings.ntp)

  val ldc = new DifficultyAdjustment(altSettings.chainSettings)

  val h1Json =
    """
      |{
      |  "extensionId" : "af4c9de8106960b47964d21e6eb2acdad7e3e168791e595f0806ebfb036ee7de",
      |  "difficulty" : "1199990374400",
      |  "votes" : "000000",
      |  "timestamp" : 1561978977137,
      |  "size" : 279,
      |  "stateRoot" : "18b7a08878f2a7ee4389c5a1cece1e2724abe8b8adc8916240dd1bcac069177303",
      |  "height" : 1,
      |  "nBits" : 100734821,
      |  "version" : 1,
      |  "id" : "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b",
      |  "adProofsRoot" : "766ab7a313cd2fb66d135b0be6662aa02dfa8e5b17342c05a04396268df0bfbb",
      |  "transactionsRoot" : "93fb06aa44413ff57ac878fda9377207d5db0e78833556b331b4d9727b3153ba",
      |  "extensionHash" : "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      |  "powSolutions" : {
      |    "pk" : "03be7ad70c74f691345cbedba19f4844e7fc514e1188a7929f5ae261d5bb00bb66",
      |    "w" : "02da9385ac99014ddcffe88d2ac5f28ce817cd615f270a0a5eae58acfb9fd9f6a0",
      |    "n" : "000000030151dc63",
      |    "d" : 46909460813884299753486408728361968139945651324239558400157099627
      |  },
      |  "adProofsId" : "cfc4af9743534b30ef38deec118a85ce6f0a3741b79b7d294f3e089c118188dc",
      |  "transactionsId" : "fc13e7fd2d1ddbd10e373e232814b3c9ee1b6fbdc4e6257c288ecd9e6da92633",
      |  "parentId" : "0000000000000000000000000000000000000000000000000000000000000000"
      |}""".stripMargin

  val h1 = Header.jsonDecoder.decodeJson(parse(h1Json).toOption.get).toOption.get

  var totalError = 0
  var maxDelay = 0

  (1 to 100).foreach { _ =>
    var blockDelay = altSettings.chainSettings.blockInterval
    val epochLength = altSettings.chainSettings.epochLength
    var price = 100000

    val medianChange = 3 // price going up 1% epoch on average
    val variance = 10 // with +-20%

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

      val newDiff = ldc.eip37Calculate(hs, epochLength)
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

  println("Planned block time: " + altSettings.chainSettings.blockInterval)
  println("Total error: " + totalError / 1000)
  println("Max delay: " + maxDelay)

}
