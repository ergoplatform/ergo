package org.ergoplatform

import akka.actor.{ActorRef, Props}
import org.ergoplatform.api.routes.{DebugApiRoute, HistoryApiRoute, StateApiRoute}
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoLocalInterface, ErgoMiner, TransactionGenerator}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import org.ergoplatform.Version.VersionString
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import scorex.core.settings.ScorexSettings

import scala.concurrent.ExecutionContextExecutor

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
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewHolder.createActor(actorSystem, ergoSettings)

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkController, settings.restApi),
    HistoryApiRoute(nodeViewHolderRef, settings.restApi, ergoSettings.nodeSettings.ADState),
    StateApiRoute(nodeViewHolderRef, settings.restApi, ergoSettings.nodeSettings.ADState),
    DebugApiRoute(settings.restApi))

  override val apiTypes: Set[Class[_]] = Set(
    classOf[UtilsApiRoute],
    classOf[PeersApiRoute],
    classOf[HistoryApiRoute],
    classOf[StateApiRoute],
    classOf[DebugApiRoute]
  )

  val minerRef: ActorRef = actorSystem.actorOf(Props(classOf[ErgoMiner], ergoSettings, nodeViewHolderRef))

  if (ergoSettings.nodeSettings.mining && ergoSettings.nodeSettings.offlineGeneration) {
    minerRef ! StartMining
  }

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[ErgoLocalInterface], nodeViewHolderRef, minerRef, ergoSettings)
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(new ErgoNodeViewSynchronizer(networkController, nodeViewHolderRef, localInterface, ErgoSyncInfoMessageSpec,
      settings.network)))

  //only a miner is generating tx load
  //    val txGen = actorSystem.actorOf(Props(classOf[TransactionGenerator], nodeViewHolderRef))
  //    txGen ! StartGeneration

}

object ErgoApp extends App {
  new ErgoApp(args).run()

  def forceStopApplication(code: Int = 1): Unit =
    new Thread(() => System.exit(code), "ergo-platform-shutdown-thread").start()
}
