package org.ergoplatform

import akka.actor.{ActorRef, Props}
import org.ergoplatform.api.routes.{HistoryApiRoute, StateApiRoute}
import org.ergoplatform.api.services.{HistoryService, StateService}
import org.ergoplatform.api.services.impl.{HistoryActorServiceImpl, StateActorServiceImpl}
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoLocalInterface, ErgoMiner, TransactionGenerator}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.state.{DigestState, UtxoState}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings

import scala.concurrent.ExecutionContext

class ErgoApp(args: Seq[String]) extends Application {
  override type P = AnyoneCanSpendProposition.type
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  implicit val ec = actorSystem.dispatcher

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(args.headOption)

  //TODO remove after Scorex update
  override implicit lazy val settings: Settings = ergoSettings.scorexSettings

  override lazy val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings),
    PeersApiRoute(peerManagerRef, networkController, settings),
    HistoryApiRoute(ErgoApp.initHistoryService(nodeViewHolderRef, ergoSettings), settings),
    StateApiRoute(ErgoApp.initStateService(nodeViewHolderRef, ergoSettings), settings))

  override lazy val apiTypes: Set[Class[_]] = Set(
    classOf[UtilsApiRoute],
    classOf[PeersApiRoute],
    classOf[HistoryApiRoute],
    classOf[StateApiRoute]
  )

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq()
  override val nodeViewHolderRef: ActorRef = ErgoNodeViewHolder.createActor(actorSystem, ergoSettings)

  override val localInterface: ActorRef = actorSystem.actorOf(
    Props(classOf[ErgoLocalInterface], nodeViewHolderRef, ergoSettings)
  )

  override val nodeViewSynchronizer: ActorRef = actorSystem.actorOf(
    Props(classOf[NodeViewSynchronizer[P, TX, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type]],
      networkController, nodeViewHolderRef, localInterface, ErgoSyncInfoMessageSpec))

  val txGen = actorSystem.actorOf(Props(classOf[TransactionGenerator], nodeViewHolderRef))
  txGen ! StartGeneration

  //todo: remove
  Thread.sleep(4000)

  if (ergoSettings.nodeSettings.mining) {
    val miner = actorSystem.actorOf(Props(classOf[ErgoMiner], ergoSettings, nodeViewHolderRef))
    miner ! StartMining
  }
}

object ErgoApp extends App {
  new ErgoApp(args).run()

  def forceStopApplication(): Unit = new Thread(() => System.exit(1), "ergo-platform-shutdown-thread").start()

  def initHistoryService(ref: ActorRef, settings: ErgoSettings)(implicit ec: ExecutionContext): HistoryService =
    if (settings.nodeSettings.ADState) {
      new HistoryActorServiceImpl[DigestState](ref)
    } else {
      new HistoryActorServiceImpl[UtxoState](ref)
    }

  def initStateService(ref: ActorRef, settings: ErgoSettings)(implicit ec: ExecutionContext): StateService =
    if (settings.nodeSettings.ADState) {
      new StateActorServiceImpl[DigestState](ref)
    } else {
      new StateActorServiceImpl[UtxoState](ref)
    }
}
