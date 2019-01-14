package org.ergoplatform

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import org.ergoplatform.api._
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local._
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.PeerFeature
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.util.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

class ErgoApp(args: Seq[String]) extends Application {
  override type TX = ErgoTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  override protected lazy val features: Seq[PeerFeature] = Seq()

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(args.headOption)

  override implicit lazy val settings: ScorexSettings = ergoSettings.scorexSettings

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider)

  val statsCollectorRef: ActorRef = ErgoStatsCollectorRef(readersHolderRef, networkControllerRef, ergoSettings, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq(
    EmissionApiRoute(ergoSettings),
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, timeProvider, settings.restApi),
    InfoRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(nodeViewHolderRef, readersHolderRef, minerRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi),
    WalletApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings)
  )

  override val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  override val nodeViewSynchronizer: ActorRef =
    ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, ErgoSyncInfoMessageSpec,
      settings.network, timeProvider)

  if (ergoSettings.nodeSettings.mining && ergoSettings.nodeSettings.offlineGeneration) {
    minerRef ! StartMining
  }

  val actorsToStop = Seq(minerRef,
    peerManagerRef,
    networkControllerRef,
    readersHolderRef,
    nodeViewSynchronizer,
    statsCollectorRef,
    nodeViewHolderRef
  )
  sys.addShutdownHook(ErgoApp.shutdown(actorSystem, actorsToStop))

  if (ergoSettings.testingSettings.transactionGeneration) {
    val txGen = TransactionGeneratorRef(nodeViewHolderRef, ergoSettings)
    txGen ! StartGeneration
  }
}

object ErgoApp extends ScorexLogging {
  def main(args: Array[String]): Unit = new ErgoApp(args).run()

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }
}
