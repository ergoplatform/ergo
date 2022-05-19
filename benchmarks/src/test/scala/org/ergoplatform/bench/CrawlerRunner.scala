package org.ergoplatform.bench

import java.io.File

import akka.actor.ActorRef
import org.ergoplatform.bench.misc.{CrawlerConfig, TempDir}
import org.ergoplatform.http.api.{BlocksApiRoute, ErgoUtilsApiRoute, InfoApiRoute, TransactionsApiRoute}
import org.ergoplatform.local.ErgoStatsCollectorRef
import org.ergoplatform.mining.ErgoMiner
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Args, ErgoSettings}
import scorex.core.api.http.ApiRoute
import scorex.core.app.Application
import scorex.core.network.{DeliveryTracker, PeerFeature}
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

class CrawlerRunner(args: Array[String]) extends Application {

  lazy val fileToSave: String = args.headOption.getOrElse("/")
  lazy val threshold: Int = args.lift(1).getOrElse("15000").toInt
  lazy val cfgPath: Option[String] = args.lift(2)
  lazy val benchConfig: CrawlerConfig = CrawlerConfig(fileToSave, threshold)
  lazy val tempDir: File = TempDir.createTempDir

  log.info(s"Temp dir is ${tempDir.getAbsolutePath}")
  log.info(s"Config is $benchConfig")

  override protected lazy val features: Seq[PeerFeature] = Seq()

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  override val ergoSettings: ErgoSettings = ErgoSettings.read(Args(cfgPath, None))

  lazy val emission = new EmissionRules(ergoSettings.chainSettings.monetary)

  override implicit lazy val settings: ScorexSettings = ergoSettings.scorexSettings

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  val minerRef: ActorRef = ErgoMiner(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider)

  private val syncTracker = ErgoSyncTracker(actorSystem, settings.network, timeProvider)

  val statsCollectorRef: ActorRef =
    ErgoStatsCollectorRef(nodeViewHolderRef, networkControllerRef, syncTracker, ergoSettings, timeProvider, LaunchParameters)


  override val apiRoutes: Seq[ApiRoute] = Seq(
    ErgoUtilsApiRoute(ergoSettings),
    InfoApiRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings))

  override val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  private val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(ergoSettings)

  override val nodeViewSynchronizer: ActorRef =
    ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, ErgoSyncInfoMessageSpec,
      ergoSettings, timeProvider, syncTracker, deliveryTracker)

}

object CrawlerRunner {
  def main(args: Array[String]): Unit = new CrawlerRunner(args).run()
}
