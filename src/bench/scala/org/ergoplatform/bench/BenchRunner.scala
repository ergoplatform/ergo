package org.ergoplatform.bench

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import org.ergoplatform.bench.misc.TempDir
import org.ergoplatform.bench.misc.Utils._
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.utils.{NetworkTimeProvider, NetworkTimeProviderSettings}
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.language.postfixOps

object BenchRunner extends ScorexLogging {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  val targetDirectory = "target/bench"

  def main(args: Array[String]): Unit = {
    new File(targetDirectory).mkdirs()
    val threshold = args.headOption.getOrElse("1000").toInt
    val fileUrl = args.lift(1).get
    val isUtxo = args.lift(2).isEmpty
    val state = if (isUtxo) StateType.Utxo else StateType.Digest
    val benchRef = BenchActor(threshold, state)
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark.")

    val settings = ErgoSettings.read(None)
    val nodeSettings = settings.nodeSettings.copy(stateType = state)

    lazy val ergoSettings: ErgoSettings = settings
      .copy(directory =  userDir.getAbsolutePath, nodeSettings = nodeSettings)

    log.info(s"Setting to be used:\n$ergoSettings")

    val ntpSettings = NetworkTimeProviderSettings("pool.ntp.org", 30 minutes, 30 seconds)
    val timeProvider = new NetworkTimeProvider(ntpSettings)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

    /**
      * It's a hack to set minimalFullBlockHeightVar to 0 and to avoid "Header Is Not Synced" error, cause
      * in our case we are considering only locally pre-generated modifiers.
      */
    nodeViewHolderRef ! GetDataFromCurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool, Unit](adjustView)

    log.info("Reading modifiers..")
    val modifiers = readModifiers(fileUrl, threshold)
    log.info("Running bench..")
    log.info(s"$threshold modifiers to go")
    runBench(benchRef, nodeViewHolderRef, modifiers)
  }

  private def runBench(benchRef: ActorRef, nodeRef: ActorRef, modifiers: Vector[ErgoPersistentModifier]): Unit = {
    benchRef ! BenchActor.Start
    modifiers.foreach(m => nodeRef ! LocallyGeneratedModifier(m))
  }

}
