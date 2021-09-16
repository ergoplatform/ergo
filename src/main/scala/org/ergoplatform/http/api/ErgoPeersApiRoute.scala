package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import org.ergoplatform.network.ErgoNodeViewSynchronizer.GetPeersFullInfo
import org.ergoplatform.network.ErgoPeerStatus
import scorex.core.api.http.{ApiResponse, PeersApiRoute}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext

class ErgoPeersApiRoute(peerManager: ActorRef,
                        networkController: ActorRef,
                        nodeViewSynchronizer: ActorRef,
                        timeProvider: NetworkTimeProvider,
                        override val settings: RESTApiSettings)
                       (override implicit val context: ActorRefFactory, override val ec: ExecutionContext)
  extends PeersApiRoute(peerManager, networkController, timeProvider, settings)(context, ec) {

  override lazy val route: Route = pathPrefix("peers") {
    allPeers ~ connectedPeers ~ blacklistedPeers ~ connect ~ peersStatus ~ fullInfo
  }

  def fullInfo: Route = (path("fullInfo") & get) {
    val result = askActor[Seq[ErgoPeerStatus]](nodeViewSynchronizer, GetPeersFullInfo)
    ApiResponse(result)
  }
}

object ErgoPeersApiRoute {
  def apply(peerManager: ActorRef,
            networkController: ActorRef,
            nodeViewSynchronizer: ActorRef,
            timeProvider: NetworkTimeProvider,
            settings: RESTApiSettings)
           (implicit context: ActorRefFactory, ec: ExecutionContext): ErgoPeersApiRoute =
    new ErgoPeersApiRoute(peerManager, networkController, nodeViewSynchronizer, timeProvider, settings)(context, ec)
}
