package org.ergoplatform

import akka.Done
import akka.actor.{ActorRef, ActorSystem, CoordinatedShutdown}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import ch.qos.logback.classic.Level
import ch.qos.logback.classic.LoggerContext
import org.ergoplatform.http._
import org.ergoplatform.http.api._
import org.ergoplatform.local._
import org.ergoplatform.mining.ErgoMiner
import org.ergoplatform.mining.ErgoMiner.StartMining
import org.ergoplatform.network.{ErgoNodeViewSynchronizer, ErgoSyncTracker}
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.slf4j.{Logger, LoggerFactory}
import org.ergoplatform.settings.{Args, ErgoSettings, NetworkType}
import scorex.core.api.http._
import scorex.core.app.ScorexContext
import scorex.core.network.NetworkController.ReceivableMessages.ShutdownNetwork
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}
import scala.io.{Codec, Source}

class ErgoApp(args: Args) extends ScorexLogging {

  log.info(s"Running with args: $args")

  private val ergoSettings: ErgoSettings = ErgoSettings.read(args)

  require(ergoSettings.scorexSettings.restApi.apiKeyHash.isDefined, "API key hash must be set")

  log.info(s"Working directory: ${ergoSettings.directory}")
  log.info(s"Secret directory: ${ergoSettings.walletSettings.secretStorage.secretDir}")

  implicit private def scorexSettings: ScorexSettings = ergoSettings.scorexSettings

  overrideLogLevel()

  implicit private val actorSystem: ActorSystem = ActorSystem(scorexSettings.network.agentName)
  implicit private val executionContext: ExecutionContext = actorSystem.dispatcher

  private val timeProvider = new NetworkTimeProvider(scorexSettings.ntp)

  private val upnpGateway: Option[UPnPGateway] =
    if (scorexSettings.network.upnpEnabled) UPnP.getValidGateway(scorexSettings.network) else None
  upnpGateway.foreach(_.addPort(scorexSettings.network.bindAddress.getPort))

  //an address to send to peers
  private val externalSocketAddress: Option[InetSocketAddress] =
    scorexSettings.network.declaredAddress orElse {
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, scorexSettings.network.bindAddress.getPort))
    }

  private val basicSpecs = {
    Seq(
      GetPeersSpec,
      new PeersSpec(scorexSettings.network.maxPeerSpecObjects),
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )
  }

  private val additionalMessageSpecs: Seq[MessageSpec[_]] = Seq(ErgoSyncInfoMessageSpec)

  private val scorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  private val peerManagerRef = PeerManagerRef(ergoSettings, scorexContext)

  private val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", ergoSettings, peerManagerRef, scorexContext)

  private val nodeViewHolderRef: ActorRef = ErgoNodeViewRef(ergoSettings, timeProvider)

  private val readersHolderRef: ActorRef = ErgoReadersHolderRef(nodeViewHolderRef)

  // Create an instance of ErgoMiner actor if "mining = true" in config
  private val minerRefOpt: Option[ActorRef] =
    if (ergoSettings.nodeSettings.mining) {
      Some(ErgoMiner(ergoSettings, nodeViewHolderRef, readersHolderRef, timeProvider))
    } else {
      None
    }

  // Create an instance of ExtraIndexer actor if "extraIndex = true" in config
  if(ergoSettings.nodeSettings.extraIndex)
    ExtraIndexerRef(ergoSettings.chainSettings, ergoSettings.cacheSettings)

  ExtraIndexerRef.setAddressEncoder(ergoSettings.addressEncoder) // initialize an accessible address encoder regardless of extra indexing being enabled

  private val syncTracker = ErgoSyncTracker(scorexSettings.network, timeProvider)
  private val statsCollectorRef: ActorRef = ErgoStatsCollectorRef(readersHolderRef, networkControllerRef, syncTracker, ergoSettings, timeProvider)
  private val deliveryTracker: DeliveryTracker = DeliveryTracker.empty(ergoSettings)

  // touch it to run preStart method of the actor which is in turn running schedulers
  ErgoNodeViewSynchronizer(
    networkControllerRef,
    nodeViewHolderRef,
    ErgoSyncInfoMessageSpec,
    ergoSettings,
    timeProvider,
    syncTracker,
    deliveryTracker
  )

  if (ergoSettings.scorexSettings.network.peerDiscovery) {
    // Launching PeerSynchronizer actor which is then registering itself at network controller
    PeerSynchronizerRef("PeerSynchronizer", networkControllerRef, peerManagerRef, scorexSettings.network)
  }

  private val apiRoutes: Seq[ApiRoute] = Seq(
    EmissionApiRoute(ergoSettings),
    ErgoUtilsApiRoute(ergoSettings),
    BlockchainApiRoute(readersHolderRef, ergoSettings),
    ErgoPeersApiRoute(peerManagerRef, networkControllerRef, syncTracker, deliveryTracker, scorexSettings.restApi),
    InfoApiRoute(statsCollectorRef, scorexSettings.restApi, timeProvider),
    BlocksApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    NipopowApiRoute(nodeViewHolderRef, readersHolderRef, ergoSettings),
    TransactionsApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings),
    WalletApiRoute(readersHolderRef, nodeViewHolderRef, ergoSettings),
    UtxoApiRoute(readersHolderRef, scorexSettings.restApi),
    ScriptApiRoute(readersHolderRef, ergoSettings),
    ScanApiRoute(readersHolderRef, ergoSettings),
    NodeApiRoute(ergoSettings)
  ) ++ minerRefOpt.map(minerRef => MiningApiRoute(minerRef, ergoSettings)).toSeq


  private val swaggerRoute = SwaggerRoute(scorexSettings.restApi, swaggerConfig)
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
    Future(upnpGateway.foreach(_.deletePort(scorexSettings.network.bindAddress.getPort))).map(_ => Done)
  }

  if (!ergoSettings.nodeSettings.stateType.requireProofs) {
    MempoolAuditorRef(nodeViewHolderRef, networkControllerRef, ergoSettings)
  }

  /**
    * Override the log level at runtime with values provided in config/user provided config.
    */
  private def overrideLogLevel() {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val root = loggerContext.getLogger(Logger.ROOT_LOGGER_NAME)

    root.setLevel(Level.toLevel(ergoSettings.scorexSettings.logging.level))
  }

  private def swaggerConfig: String = Source.fromResource("api/openapi.yaml")(Codec.UTF8).getLines.mkString("\n")

  private def run(): Future[ServerBinding] = {
    require(scorexSettings.network.agentName.length <= ErgoApp.ApplicationNameLimit)

    log.info(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.info(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.info(s"RPC is allowed at ${scorexSettings.restApi.bindAddress.toString}")

    if (ergoSettings.chainSettings.reemission.checkReemissionRules) {
      log.info("Checking re-emission rules enabled")
      log.info(s"EIP27 activation height: " + ergoSettings.chainSettings.reemission.activationHeight)
    }

    val bindAddress = scorexSettings.restApi.bindAddress

    Http().newServerAt(bindAddress.getAddress.getHostAddress, bindAddress.getPort).bindFlow(httpService.compositeRoute)
  }
}

object ErgoApp extends ScorexLogging {
  val ApplicationNameLimit: Int = 50
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

  def main(args: Array[String]): Unit = {
    argParser.parse(args, Args()).foreach { argsParsed =>
      new ErgoApp(argsParsed).run()
    }
  }

}
