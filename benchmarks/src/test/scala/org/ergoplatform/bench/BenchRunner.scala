package org.ergoplatform.bench

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import org.ergoplatform.bench.misc.TempDir
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.{ErgoNodeViewRef, NVBenchmark}
import org.ergoplatform.settings.{Args, ErgoSettings}
import scorex.core.NodeViewHolder.CurrentView
import scorex.core.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import scorex.core.utils.{NetworkTimeProvider, NetworkTimeProviderSettings}
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._
import scala.language.postfixOps

object BenchRunner extends ScorexLogging with NVBenchmark {

  implicit val system: ActorSystem = ActorSystem("bench")
  implicit val ec: ExecutionContextExecutor = system.dispatcher

  def main(args: Array[String]): Unit = {
    val threshold = args.headOption.getOrElse("1000").toInt
    val isUtxo = args.lift(2).isEmpty
    val state = if (isUtxo) StateType.Utxo else StateType.Digest
    val benchRef = BenchActor(threshold, state)
    val userDir = TempDir.createTempDir

    log.info(s"User dir is $userDir")
    log.info("Starting benchmark.")

    val realNetworkSettings = ErgoSettings.read(Args(Some("src/main/resources/application.conf"), None))
    val nodeSettings = realNetworkSettings.nodeSettings.copy(stateType = state)

    lazy val ergoSettings: ErgoSettings = realNetworkSettings
      .copy(directory =  userDir.getAbsolutePath, nodeSettings = nodeSettings)

    log.info(s"Setting that being used:")
    log.info(s"$ergoSettings")

    val ntpSettings = NetworkTimeProviderSettings("pool.ntp.org", 30 minutes, 30 seconds)
    val timeProvider = new NetworkTimeProvider(ntpSettings)

    val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

    /**
      * It's a hack to set minimalFullBlockHeightVar to 0 and to avoid "Header Is Not Synced" error, cause
      * in our case we are considering only locally pre-generated modifiers.
      */
    nodeViewHolderRef ! GetDataFromCurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool, Unit](adjust)

    log.info("Starting to read modifiers.")
    log.info("Finished read modifiers, starting to bench.")
    log.info(s"$threshold modifiers to go")
    runBench(benchRef, nodeViewHolderRef, (readHeaders ++ readExtensions ++ readPayloads).toVector)
  }

  private def adjust(v: CurrentView[ErgoHistory, ErgoState[_], ErgoWallet, ErgoMemPool]): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(v.history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true: Boolean)
    ()
  }

  private def runBench(benchRef: ActorRef, nodeRef: ActorRef, modifiers: Vector[ErgoPersistentModifier]): Unit = {
    benchRef ! BenchActor.Start
    modifiers.foreach { m => nodeRef ! LocallyGeneratedModifier(m) }
  }

}
