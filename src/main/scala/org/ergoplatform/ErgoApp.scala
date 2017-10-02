package org.ergoplatform

import akka.actor.{ActorRef, Props}
import org.ergoplatform.local.ErgoMiner.StartMining
import org.ergoplatform.local.TransactionGenerator.StartGeneration
import org.ergoplatform.local.{ErgoLocalInterface, ErgoMiner, TransactionGenerator}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.nodeView.ErgoNodeViewHolder
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.NodeViewSynchronizer
import scorex.core.network.message.MessageSpec
import scorex.core.settings.Settings

class ErgoApp(args: Seq[String]) extends Application {
  override type P = AnyoneCanSpendProposition.type
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(args.headOption)

  //TODO remove after Scorex update
  override implicit lazy val settings: Settings = ergoSettings.scorexSettings

  override lazy val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings),
    PeersApiRoute(peerManagerRef, networkController, settings))

  override lazy val apiTypes: Set[Class[_]] = Set(classOf[UtilsApiRoute], classOf[PeersApiRoute])

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
  Thread.sleep(1000)

  val miner = actorSystem.actorOf(Props(classOf[ErgoMiner], ergoSettings, nodeViewHolderRef))
  miner ! StartMining
}

object ErgoApp extends App {
  new ErgoApp(args).run()
  def forceStopApplication(): Unit = new Thread(() => System.exit(1), "ergo-platform-shutdown-thread").start()
}
