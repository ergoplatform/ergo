package scorex.core.app

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route}
import org.ergoplatform.ErgoApp
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.{ApiErrorHandler, ApiRejectionHandler, ApiRoute, CompositeHttpService}
import scorex.core.network._
import scorex.core.network.message._
import scorex.core.network.peer.PeerManagerRef
import scorex.core.settings.ScorexSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext

trait Application extends ScorexLogging {

  //settings
  val ergoSettings: ErgoSettings
  implicit val scorexSettings: ScorexSettings

  //api
  val apiRoutes: Seq[ApiRoute]

  implicit def exceptionHandler: ExceptionHandler = ApiErrorHandler.exceptionHandler
  implicit def rejectionHandler: RejectionHandler = ApiRejectionHandler.rejectionHandler

  protected implicit lazy val actorSystem: ActorSystem = ActorSystem(scorexSettings.network.agentName)
  implicit val executionContext: ExecutionContext = actorSystem.dispatchers.lookup("scorex.executionContext")

  protected val features: Seq[PeerFeature]
  protected val additionalMessageSpecs: Seq[MessageSpec[_]]

  //p2p
  private val upnpGateway: Option[UPnPGateway] = if (scorexSettings.network.upnpEnabled) UPnP.getValidGateway(scorexSettings.network) else None
  // TODO use available port on gateway instead settings.network.bindAddress.getPort
  upnpGateway.foreach(_.addPort(scorexSettings.network.bindAddress.getPort))

  private lazy val basicSpecs = {
    Seq(
      GetPeersSpec,
      new PeersSpec(scorexSettings.network.maxPeerSpecObjects),
      InvSpec,
      RequestModifierSpec,
      ModifiersSpec
    )
  }

  val nodeViewHolderRef: ActorRef
  val nodeViewSynchronizer: ActorRef

  /** API description in openapi format in YAML or JSON */
  val swaggerConfig: String

  val timeProvider = new NetworkTimeProvider(scorexSettings.ntp)

  //an address to send to peers
  lazy val externalSocketAddress: Option[InetSocketAddress] = {
    scorexSettings.network.declaredAddress orElse {
      // TODO use available port on gateway instead settings.bindAddress.getPort
      upnpGateway.map(u => new InetSocketAddress(u.externalAddress, scorexSettings.network.bindAddress.getPort))
    }
  }

  val scorexContext = ScorexContext(
    messageSpecs = basicSpecs ++ additionalMessageSpecs,
    upnpGateway = upnpGateway,
    timeProvider = timeProvider,
    externalNodeAddress = externalSocketAddress
  )

  val peerManagerRef = PeerManagerRef(ergoSettings, scorexContext)

  val networkControllerRef: ActorRef = NetworkControllerRef(
    "networkController", ergoSettings, peerManagerRef, scorexContext)

  val peerSynchronizer: ActorRef =
    PeerSynchronizerRef("PeerSynchronizer", networkControllerRef, peerManagerRef, scorexSettings.network)

  lazy val combinedRoute: Route = CompositeHttpService(actorSystem, apiRoutes, scorexSettings.restApi, swaggerConfig).compositeRoute

  def run(): Unit = {
    val applicationNameLimit: Int = 50
    require(scorexSettings.network.agentName.length <= applicationNameLimit)

    log.debug(s"Available processors: ${Runtime.getRuntime.availableProcessors}")
    log.debug(s"Max memory available: ${Runtime.getRuntime.maxMemory}")
    log.debug(s"RPC is allowed at ${scorexSettings.restApi.bindAddress.toString}")

    val bindAddress = scorexSettings.restApi.bindAddress

    Http().newServerAt(bindAddress.getAddress.getHostAddress, bindAddress.getPort).bindFlow(combinedRoute)

    //on unexpected shutdown
    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() {
        log.error("Unexpected shutdown")
        ErgoApp.shutdownSystem()
      }
    })
  }
}
