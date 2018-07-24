package org.ergoplatform.bench

import java.io.FileOutputStream

import org.ergoplatform.bench.misc.ModifierWriter
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.ErgoSettings
import scorex.core.settings.ScorexSettings
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global

object HistoryExtractor extends ScorexLogging {

  def main(args: Array[String]): Unit = {

    lazy val cfgPath: Option[String] = args.headOption
    lazy val outputFile: String = args.lift(1).getOrElse("output.dat")
    lazy val ergoSettings: ErgoSettings = ErgoSettings.read(cfgPath)
    lazy val settings: ScorexSettings = ergoSettings.scorexSettings

    val timeProvider = new NetworkTimeProvider(settings.ntp)
    val os = new FileOutputStream(outputFile)
    val h = ErgoHistory.readOrGenerate(ergoSettings, timeProvider)
    val wholeChain = h.chainToHeader(None, h.bestHeaderOpt.get)

    var counter = 0
    wholeChain._2.headers.take(10000).foreach { header =>
      counter += 1
      if (counter % 100 == 0) { log.info(s"Processed $counter blocks.")}
      val b = h.getFullBlock(header).get
      ModifierWriter.write(b.header)(os)
      ModifierWriter.write(b.blockTransactions)(os)
      b.adProofs.foreach { p => ModifierWriter.write(p)(os) }
      os.flush()
    }
    os.close()
  }
}
