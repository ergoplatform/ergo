package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Random

object DifficultyControlChecker extends App with ErgoGenerators {

  val baseHeader = invalidHeaderGen.sample.get
  val chainSettings = ErgoSettings.read(None).chainSettings.copy(epochLength = 1)
  val difficultyControl = new LinearDifficultyControl(chainSettings.blockInterval,
    chainSettings.useLastEpochs, chainSettings.epochLength)

  blockchainSimulator(difficultyControl, baseHeader.copy(height = 0, timestamp = 0, interlinks = Seq(), nBits = 16842752))

  /**
    * Generate blockchain starting from initial header with specified difficulty control and measure mean time interval between blocks
    *
    * @param difficultyControl
    * @param initialHeader
    */
  def blockchainSimulator(difficultyControl: LinearDifficultyControl, initialHeader: Header): Unit = {
    // number of blocks in simulated chain
    val chainLength = difficultyControl.useLastEpochs * difficultyControl.epochLength * 2
    // simulated time for one hash calculation
    val timeForOneHash = difficultyControl.desiredInterval.toMillis / 60

    val curChain = mutable.Map[Int, Header](initialHeader.height -> initialHeader)

    @tailrec
    def genchain(curHeight: Int): mutable.Map[Int, Header] = {
      if (curHeight >= chainLength) {
        curChain
      } else {
        val lastHeader = curChain(curHeight)
        val requiredDifficulty = requiredDifficultyAfter(curChain)
        val target = 1.toDouble / requiredDifficulty.toDouble

        @tailrec
        def simulateTimeDiff(currentDiff: Long = 0): Long = {
          val hit = Random.nextDouble()
          if (hit < target) {
            currentDiff + timeForOneHash
          } else {
            simulateTimeDiff(currentDiff + timeForOneHash)
          }
        }

        val timeDiff = simulateTimeDiff()
        val newHeader = lastHeader.copy(timestamp = lastHeader.timestamp + timeDiff, height = curHeight + 1)
        curChain(newHeader.height) = newHeader
        genchain(curHeight + 1)
      }
    }

    genchain(initialHeader.height)


    def requiredDifficultyAfter(blockchain: mutable.Map[Int, Header]): Difficulty = {
      val parent: Header = blockchain.head._2
      val parentHeight = parent.height
      val heights = difficultyControl.previousHeadersRequiredForRecalculation(parentHeight + 1)
        .ensuring(_.last == parentHeight)
      if (heights.lengthCompare(1) == 0) {
        difficultyControl.calculate(Seq(parent))
      } else {
        val headersToCalculate = heights.map(h => blockchain(h))
        difficultyControl.calculate(headersToCalculate)
      }
    }
  }

  def printTestnetData(): Unit = {
    val baseHeader = invalidHeaderGen.sample.get
    val chainSettings = ErgoSettings.read(None).chainSettings.copy(epochLength = 1)
    val difficultyControl = new LinearDifficultyControl(chainSettings.blockInterval,
      chainSettings.useLastEpochs, chainSettings.epochLength)

    val headers = Source.fromResource("difficulty.csv").getLines().toSeq.tail.map { line =>
      val l = line.split(",")
      baseHeader.copy(height = l(0).toInt,
        timestamp = l(1).toLong,
        interlinks = Seq(),
        nBits = RequiredDifficulty.encodeCompactBits(BigInt(l(2))))
    }

    case class DiffRow(height: Int, requiredDifficulty: BigInt, timeDiff: Long, timestamp: Long) {
      val realDifficulty = requiredDifficulty * chainSettings.blockInterval.toMillis * chainSettings.epochLength / timeDiff

      override def toString: String = s"$height,$requiredDifficulty,$realDifficulty,$timeDiff"
    }

    val data: Seq[DiffRow] = headers.sliding(2).map { p =>
      val start = p.head
      val end = p.last

      DiffRow(end.height, end.requiredDifficulty, end.timestamp - start.timestamp, end.timestamp)
    }.toSeq

    //header in more or less stable situation
    val last1000 = data.takeRight(1000)

    println(s"height,requiredDifficulty,realDifficulty,timeDiff")
    last1000.foreach(d => println(d))
  }

}
