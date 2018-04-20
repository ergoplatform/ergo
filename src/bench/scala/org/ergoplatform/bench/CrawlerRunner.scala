package org.ergoplatform.bench

import akka.actor.{ActorRef, Props}
import org.ergoplatform.api.routes.{BlocksApiRoute, InfoRoute, TransactionsApiRoute}
import org.ergoplatform.bench.misc.CrawlerConfig
import org.ergoplatform.local.{ErgoMinerRef, ErgoStatsCollectorRef}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

class CrawlerRunner(args: Array[String]) extends Application {

  override type P = AnyoneCanSpendProposition.type
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  lazy val fileToSave = args.headOption.getOrElse("/")
  lazy val threshold: Int = args.lift(1).getOrElse("15000").toInt
  lazy val cfgPath: Option[String] = args.lift(2)
  lazy val benchConfig = CrawlerConfig(fileToSave, threshold)
  lazy val tempDir = TempDir.createTempDir

  log.info(s"Temp dir is ${tempDir.getAbsolutePath}")
  log.info(s"Config is $benchConfig")

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(cfgPath)

  override implicit lazy val settings: ScorexSettings = ergoSettings.scorexSettings

  val crawlerRef = actorSystem.actorOf(Props.apply(classOf[CrawlerActor], benchConfig))

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  val nodeId: Array[Byte] = Algos.hash(ergoSettings.scorexSettings.network.nodeName).take(5)

  val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, nodeId, timeProvider)

  val statsCollectorRef: ActorRef = ErgoStatsCollectorRef(nodeViewHolderRef, peerManagerRef, ergoSettings, timeProvider)

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    InfoRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(readersHolderRef, minerRef, ergoSettings, nodeId),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi))

  override val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  override val nodeViewSynchronizer: ActorRef = ErgoNodeViewSynchronizer(
    networkControllerRef,
    nodeViewHolderRef, ErgoSyncInfoMessageSpec, settings.network, timeProvider)

}

object CrawlerRunner {
  def main(args: Array[String]): Unit = new CrawlerRunner(args).run
}
