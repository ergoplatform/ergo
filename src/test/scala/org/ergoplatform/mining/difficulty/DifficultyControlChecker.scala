package org.ergoplatform.mining.difficulty

import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoGenerators

import scala.io.Source

object DifficultyControlChecker extends App with ErgoGenerators {


  def printTestnetData(): Unit = {
    val baseHeader = invalidHeaderGen.sample.get
    val chainSettings = ErgoSettings.read(None).chainSettings.copy(epochLength = 1)
    val difficultyCalculator = new LinearDifficultyControl(chainSettings.blockInterval,
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
