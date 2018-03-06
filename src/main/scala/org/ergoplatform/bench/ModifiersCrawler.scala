package org.ergoplatform.bench

import akka.actor.ActorRef
import org.ergoplatform.api.routes.{BlocksApiRoute, InfoRoute, TransactionsApiRoute}
import org.ergoplatform.local.{ErgoMinerRef, ErgoStatsCollectorRef}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.{ApiRoute, PeersApiRoute, UtilsApiRoute}
import scorex.core.app.Application
import scorex.core.network.message.MessageSpec
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.state.StateReader
import scorex.core.{LocalInterface, ModifierId}

import scala.concurrent.ExecutionContextExecutor
import scala.io.Source

class ModifiersCrawler(args: Array[String]) extends Application {

  override type P = AnyoneCanSpendProposition.type
  override type TX = AnyoneCanSpendTransaction
  override type PMOD = ErgoPersistentModifier
  override type NVHT = ErgoNodeViewHolder[_]

  lazy val fileToSave = args.headOption.getOrElse("/")
  lazy val threshold: Int = args.lift(1).getOrElse("15000").toInt
  lazy val benchConfig = BenchmarkConfig(fileToSave, threshold)
  lazy val tempDir = TempDir.createTempDir

  log.info(s"Temp dir is ${tempDir.getAbsolutePath}")
  log.info(s"Config is $benchConfig")

  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  lazy val ergoSettings: ErgoSettings = ErgoSettings.read(Some("/Users/daron/IdeaProjects/ergo/src/main/resources/application.conf"))

  override implicit lazy val settings: ScorexSettings = ergoSettings.scorexSettings

  override protected lazy val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)
  override val nodeViewHolderRef: ActorRef = BenchmarkErgoNodeViewHolderRef(ergoSettings, timeProvider, benchConfig)
  val nodeId: Array[Byte] = Algos.hash(ergoSettings.scorexSettings.network.nodeName).take(5)

  val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  val minerRef: ActorRef = ErgoMinerRef(ergoSettings, nodeViewHolderRef, readersHolderRef, nodeId, timeProvider)

  override val localInterface: ActorRef = ErgoStatsCollectorRef(nodeViewHolderRef, peerManagerRef, ergoSettings, timeProvider)
  //actorSystem.actorOf(Props(classOf[EmptyLocalInterface], nodeViewHolderRef))

  override val apiRoutes: Seq[ApiRoute] = Seq(
    UtilsApiRoute(settings.restApi),
    PeersApiRoute(peerManagerRef, networkControllerRef, settings.restApi),
    InfoRoute(localInterface, settings.restApi, timeProvider),
    BlocksApiRoute(readersHolderRef, minerRef, ergoSettings, nodeId),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi))

  override val swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  override val nodeViewSynchronizer: ActorRef =
    ErgoNodeViewSynchronizer(networkControllerRef, nodeViewHolderRef, localInterface,
      ErgoSyncInfoMessageSpec, settings.network, timeProvider)

}

object ModifiersCrawler {
  def main(args: Array[String]): Unit = new ModifiersCrawler(args).run
}

class EmptyLocalInterface(ref: ActorRef) extends LocalInterface[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier] {

  override def preStart(): Unit = {}

  override def receive: Receive = {
    case _ => ()
  }

  override protected def onChangedState(stateReader: StateReader): Unit = {}

  override protected def onStartingPersistentModifierApplication(pmod: ErgoPersistentModifier): Unit = {}

  override protected def onFailedTransaction(tx: AnyoneCanSpendTransaction): Unit = {}

  override protected def onSuccessfulTransaction(tx: AnyoneCanSpendTransaction): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = {}

  override protected def onBetterNeighbourAppeared(): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSemanticallySuccessfulModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSemanticallyFailedModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {}

  override val viewHolderRef: ActorRef = ref
}
