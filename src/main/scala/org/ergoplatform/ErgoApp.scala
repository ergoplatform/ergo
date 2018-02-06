package org.ergoplatform

import akka.actor.{ActorRef, Props}
import org.ergoplatform.api.routes._
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoLocalInterface, ErgoMiner, TransactionGenerator}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoReadersHolder}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

class ErgoApp(args: Seq[String]) extends Application {
  override type P = AnyoneCanSpendProposition.type
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(args.headOption)

  //TODO remove after Scorex update
  override implicit lazy val settings: ScorexSettings = ergoSettings.scorexSettings

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewHolder.createActor(actorSystem, ergoSettings, timeProvider)
  val nodeId: Array[Byte] = Algos.hash(ergoSettings.scorexSettings.network.nodeName).take(5)

  val readersHolderRef: ActorRef = actorSystem.actorOf(Props(classOf[ErgoReadersHolder], nodeViewHolderRef))

  val minerRef: ActorRef = actorSystem.actorOf(Props(classOf[ErgoMiner], ergoSettings, nodeViewHolderRef,
    readersHolderRef, nodeId, timeProvider))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    InfoRoute(readersHolderRef, minerRef, peerManagerRef, ergoSettings.nodeSettings.ADState, settings.restApi, nodeId),
    BlocksApiRoute(readersHolderRef, minerRef, ergoSettings, nodeId, ergoSettings.nodeSettings.ADState),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi, ergoSettings.nodeSettings.ADState))

  override val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  if (ergoSettings.nodeSettings.mining && ergoSettings.nodeSettings.offlineGeneration) {
    minerRef ! StartMining
  }

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[ErgoLocalInterface], nodeViewHolderRef)
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, localInterface, ErgoSyncInfoMessageSpec,
      settings.network, timeProvider)))

  if (ergoSettings.testingSettings.transactionGeneration) {
    val txGen = actorSystem.actorOf(Props(classOf[TransactionGenerator], nodeViewHolderRef, ergoSettings.testingSettings))
    txGen ! StartGeneration
  }

}

object ErgoApp {

  def main(args: Array[String]): Unit = new ErgoApp(args).run()

  def forceStopApplication(code: Int = 1) = {
    new Thread(() => System.exit(code), "ergo-platform-shutdown-thread").start()
    throw new Error("Exit")
  }

}
