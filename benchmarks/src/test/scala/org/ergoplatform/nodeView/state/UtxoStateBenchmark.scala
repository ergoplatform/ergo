package org.ergoplatform.nodeView.state

import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{BlockTransactions, Extension, Header}
import org.ergoplatform.nodeView.NVBenchmark
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.HistoryTestHelpers

object UtxoStateBenchmark extends HistoryTestHelpers with NVBenchmark with App {

  val WarmupRuns = 2

  override def main(args: Array[String]): Unit = {

    val realNetworkSetting = ErgoSettings.read(Some("src/main/resources/application.conf"))

    val resourceUrlPrefix = "https://github.com/ergoplatform/static-data/raw/master/v2"

    val headers: Seq[Header] = readModifiers[Header](s"$resourceUrlPrefix/headers.dat")
    val payloads: Seq[BlockTransactions] = readModifiers[BlockTransactions](s"$resourceUrlPrefix/payloads.dat")
    val extensions: Seq[Extension] = readModifiers[Extension](s"$resourceUrlPrefix/extensions.dat")

    val blocks = headers.zip(payloads).zip(extensions).map { case ((h, txs), ext) => ErgoFullBlock(h, txs, ext, None) }

    val transactionsQty = payloads.flatMap(_.transactions).size

    def bench(benchCase: String)(mods: Seq[ErgoPersistentModifier]): String = {
      val state = ErgoState.generateGenesisUtxoState(createTempDir, StateConstants(None, realNetworkSetting))._1
      val et = time {
        mods.foldLeft(state) { case (st, mod) =>
          st.applyModifier(mod).get
        }
      }
      s"Performance of `$benchCase` ($transactionsQty transaction): $et ms"
    }

    (0 to WarmupRuns).foreach(bench("")(blocks))

    val fullBlocksApplication = bench("modifiers application")(blocks)

    println(fullBlocksApplication)

    System.exit(0)

  }

}