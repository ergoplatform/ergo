package org.ergoplatform.bench

import akka.actor.{ActorRef, Props}
import org.ergoplatform.api.{BlocksApiRoute, InfoRoute, TransactionsApiRoute}
import org.ergoplatform.bench.misc.CrawlerConfig
import org.ergoplatform.local.{ErgoMinerRef, ErgoStatsCollectorRef}
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.PeerFeature
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

class CrawlerRunner(args: Array[String]) extends Application {

  override type TX = ErgoTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  lazy val fileToSave = args.headOption.getOrElse("/")
  lazy val threshold: Int = args.lift(1).getOrElse("15000").toInt
  lazy val cfgPath: Option[String] = args.lift(2)
  lazy val benchConfig = CrawlerConfig(fileToSave, threshold)
  lazy val tempDir = TempDir.createTempDir

  log.info(s"Temp dir is ${tempDir.getAbsolutePath}")
  log.info(s"Config is $benchConfig")

  override protected lazy val features: Seq[PeerFeature] = Seq()

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(cfgPath)

  lazy val emission = new CoinsEmission(ergoSettings.chainSettings.monetary)

  override implicit lazy val settings: ScorexSettings = ergoSettings.scorexSettings

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider, emission)

  val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)


  val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider, emission)

  val statsCollectorRef: ActorRef = ErgoStatsCollectorRef(nodeViewHolderRef, peerManagerRef, ergoSettings, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    InfoRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(readersHolderRef, minerRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi))

  override val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  override val nodeViewSynchronizer: ActorRef =
    ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, ErgoSyncInfoMessageSpec,
      settings.network, timeProvider)

}

object CrawlerRunner {
  def main(args: Array[String]): Unit = new CrawlerRunner(args).run
}
