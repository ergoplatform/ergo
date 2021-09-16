package org.ergoplatform

import akka.Done
import akka.actor.{ActorRef, ActorSystem, CoordinatedShutdown}
import akka.http.scaladsl.Http
import akka.stream.SystemMaterializer
import org.ergoplatform.http._
import org.ergoplatform.http.api._
import org.ergoplatform.local._
import org.ergoplatform.mining.ErgoMiner
import org.ergoplatform.mining.ErgoMiner.StartMining
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ModeFeature}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{Args, ErgoSettings, NetworkType}
import scorex.core.api.http._
import scorex.core.app.{Application, ScorexContext}
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

class ErgoApp(args: Args) extends ScorexLogging {

  log.info(s"Running with args: $args")

  private val ergoSettings: ErgoSettings = ErgoSettings.read(args)

  require(ergoSettings.scorexSettings.restApi.apiKeyHash.isDefined, "API key hash must be set")

  log.info(s"Working directory: ${ergoSettings.directory}")
  log.info(s"Secret directory: ${ergoSettings.walletSettings.secretStorage.secretDir}")

  implicit private def settings: ScorexSettings = ergoSettings.scorexSettings

  implicit private val actorSystem: ActorSystem = ActorSystem(settings.network.agentName)
  implicit private val executionContext: ExecutionContext = actorSystem.dispatcher

  private val features: Seq[PeerFeature] = Seq(ModeFeature(ergoSettings.nodeSettings))
  private val featureSerializers: PeerFeature.Serializers = features.map(f => f.featureId -> f.serializer).toMap

  private val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)

  private val upnpGateway: Option[UPnPGateway] =
    if (settings.network.upnpEnabled) UPnP.getValidGateway(settings.network) else None
  upnpGateway.foreach(_.addPort(settings.network.bindAddress.getPort))

  //an address to send to peers
  private val externalSocketAddress: Option[InetSocketAddress] =
    settings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, settings.network.bindAddress.getPort))
    }

  private val basicSpecs = {
    val invSpec = new InvSpec(settings.network.maxInvObjects)
    val requestModifierSpec = new RequestModifierSpec(settings.network.maxInvObjects)
    val modifiersSpec = new ModifiersSpec(settings.network.maxPacketSize)
    Seq(
      GetPeersSpec,
      new PeersSpec(featureSerializers, settings.network.maxPeerSpecObjects),
      invSpec,
      requestModifierSpec,
      modifiersSpec
    )
  }

  private val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)

  private val scorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    features = features,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  private val peerManagerRef = PeerManagerRef(settings, scorexContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", settings.network, peerManagerRef, scorexContext)

  private val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  private val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  // Create an instance of ErgoMiner actor if "mining = true" in config
  private val minerRefOpt: Option[ActorRef] =
    if (ergoSettings.nodeSettings.mining) {
      Some(ErgoMiner(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider))
    } else {
      None
    }

  private val statsCollectorRef: ActorRef =
    ErgoStatsCollectorRef(readersHolderRef, networkControllerRef, ergoSettings, timeProvider)

  private val nodeViewSynchronizerRef = ErgoNodeViewSynchronizer(
    networkControllerRef,
    nodeViewHolderRef,
    ErgoSyncInfoMessageSpec,
    ergoSettings,
    timeProvider)

  // Launching PeerSynchronizer actor which is then registering itself at network controller
  PeerSynchronizerRef("PeerSynchronizer", networkControllerRef, peerManagerRef, settings.network, featureSerializers)

  private val apiRoutes: Seq[ApiRoute] = Seq(
    EmissionApiRoute(ergoSettings),
    ErgoUtilsApiRoute(ergoSettings),
    ErgoPeersApiRoute(peerManagerRef, networkControllerRef, nodeViewSynchronizerRef, timeProvider, settings.restApi),
    InfoApiRoute(statsCollectorRef, settings.restApi, timeProvider),
    BlocksApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    NipopowApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, settings.restApi),
    WalletApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings),
    UtxoApiRoute(readersHolderRef, settings.restApi),
    ScriptApiRoute(readersHolderRef, ergoSettings),
    ScanApiRoute(readersHolderRef, ergoSettings),
    NodeApiRoute(ergoSettings)
  ) ++ minerRefOpt.map(minerRef => MiningApiRoute(minerRef, ergoSettings)).toSeq


  private val swaggerRoute = SwaggerRoute(settings.restApi, swaggerConfig)
  private val panelRoute = NodePanelRoute()

  private val httpService = ErgoHttpService(apiRoutes, swaggerRoute, panelRoute)

  // Run mining immediately, i.e. without syncing if mining = true and offlineGeneration = true
  // Useful for local blockchains (devnet)
  if (ergoSettings.nodeSettings.mining && ergoSettings.nodeSettings.offlineGeneration) {
    require(minerRefOpt.isDefined, "Miner does not exist but mining = true in config")
    log.info(s"Starting mining with offlineGeneration")
    minerRefOpt.get ! StartMining
  }

  private val coordinatedShutdown = CoordinatedShutdown(actorSystem)
  coordinatedShutdown.addActorTerminationTask(
    CoordinatedShutdown.PhaseBeforeServiceUnbind,
    s"closing-network",
    networkControllerRef,
    Some(ShutdownNetwork)
  )

  coordinatedShutdown.addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, "stop-upnpGateway") { () =>
    Future(upnpGateway.foreach(_.deletePort(settings.network.bindAddress.getPort))).map(_ => Done)
  }

  if (!ergoSettings.nodeSettings.stateType.requireProofs) {
    MempoolAuditorRef(nodeViewHolderRef, networkControllerRef, ergoSettings)
  }

  private def swaggerConfig: String = Source.fromResource("api/openapi.yaml").getLines.mkString("\n")

  private def run(): Unit = {
    require(settings.network.agentName.length <= Application.ApplicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${settings.restApi.bindAddress.toString}")

    implicit val mat: SystemMaterializer = SystemMaterializer.get(actorSystem)
    val bindAddress = settings.restApi.bindAddress

    Http().newServerAt(bindAddress.getAddress.getHostAddress, bindAddress.getPort).bindFlow(httpService.compositeRoute)
  }
}

object ErgoApp extends ScorexLogging {

  val argParser = new scopt.OptionParser[Args]("ergo") {
      opt[String]("config")
        .abbr("c")
        .action((x, c) => c.copy(userConfigPathOpt = Some(x)))
        .text("location of ergo node configuration")
        .optional()
      opt[Unit]("devnet")
        .action((_, c) => c.copy(networkTypeOpt = Some(NetworkType.DevNet)))
        .text("set network to devnet")
        .optional()
      opt[Unit]("testnet")
        .action((_, c) => c.copy(networkTypeOpt = Some(NetworkType.TestNet)))
        .text("set network to testnet")
        .optional()
      opt[Unit]("mainnet")
        .action((_, c) => c.copy(networkTypeOpt = Some(NetworkType.MainNet)))
        .text("set network to mainnet")
        .optional()
      help("help").text("prints this usage text")
  }

  /** Internal failure causing shutdown */
  case object InternalShutdown extends CoordinatedShutdown.Reason

  /** Intentional user invoked remote shutdown */
  case object RemoteShutdown extends CoordinatedShutdown.Reason

  /** Exception that triggers proper system shutdown */
  case class CriticalSystemException(message: String) extends Exception(message)

  /** hard application exit in case actor system is not started yet*/
  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  /** The only proper way of application shutdown after actor system initialization */
  def shutdownSystem(reason: CoordinatedShutdown.Reason = InternalShutdown)
                    (implicit system: ActorSystem): Future[Done] =
    CoordinatedShutdown(system).run(reason)

  def main(args: Array[String]): Unit = argParser.parse(args, Args()) match {
    case Some(argsParsed) => new ErgoApp(argsParsed).run()
    case None => // Error message will be displayed when arguments are bad
  }

}
