package org.ergoplatform.nodeView.wallet

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.Utils
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.NVBenchmark
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{ReadBalances, ScanOnChain}
import org.ergoplatform.nodeView.wallet.persistence.RegistryDigest
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import org.ergoplatform.wallet.boxes.{ChainStatus, DefaultBoxSelector}
import scorex.testkit.utils.FileUtils

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object WalletBench
  extends App
    with NVBenchmark
    with ErgoTransactionGenerators
    with FileUtils {

  val WarmupRuns = 2
  val BenchRuns = 10

  private val system = ActorSystem()

  val walletRef: ActorRef = system.actorOf(Props(classOf[ErgoWalletActor], settings, DefaultBoxSelector))

  private implicit val timeout: Timeout = 10.minutes

  private val numBlocks = 20
  private val numTxs = 20

  private def blocks: Seq[ErgoFullBlock] = (0 to numBlocks).flatMap { _ =>
    invalidErgoFullBlockGen(defaultMinerPk, numTxs).sample
  }

  private def bench(blocks: Seq[ErgoFullBlock]): Unit = {
    blocks.foreach(walletRef ! ScanOnChain(_))
    val balancesF: Future[RegistryDigest] = (walletRef ? ReadBalances(ChainStatus.OnChain)).mapTo[RegistryDigest]
    Await.result(balancesF, 10.minutes)
  }

  (0 until WarmupRuns).foreach(_ => bench(blocks))

  val accEt = (0 until BenchRuns).foldLeft(0D) { (acc, _) =>
    val benchBlockSet = blocks
    acc + Utils.time(bench(benchBlockSet))
  }
  val avgEt = accEt / BenchRuns

  println(s"Elapsed time: ($numBlocks blocks x $numTxs txs) - $avgEt ms")

  val testDataDir = new File(settings.directory)
  org.apache.commons.io.FileUtils.deleteDirectory(testDataDir)

  system.terminate()
  sys.exit()

}
