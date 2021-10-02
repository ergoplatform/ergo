package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import org.ergoplatform.network.ErgoSyncTracker
import scorex.core.api.http.{ApiResponse, PeersApiRoute}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class ErgoPeersApiRoute(peerManager: ActorRef,
                        networkController: ActorRef,
                        syncTracker: ErgoSyncTracker,
                        timeProvider: NetworkTimeProvider,
                        override val settings: RESTApiSettings)
                       (override implicit val context: ActorRefFactory, override val ec: ExecutionContext)
  extends PeersApiRoute(peerManager, networkController, timeProvider, settings)(context, ec) {

  override implicit lazy val timeout: Timeout = Timeout(1.minute)

  override lazy val route: Route = pathPrefix("peers") {
    allPeers ~ connectedPeers ~ blacklistedPeers ~ connect ~ peersStatus ~ fullInfo
  }

  def fullInfo: Route = (path("fullInfo") & get) {
    ApiResponse(syncTracker.fullInfo())
  }

}

object ErgoPeersApiRoute {

  def apply(peerManager: ActorRef,
            networkController: ActorRef,
            syncTracker: ErgoSyncTracker,
            timeProvider: NetworkTimeProvider,
            settings: RESTApiSettings)
           (implicit context: ActorRefFactory, ec: ExecutionContext): ErgoPeersApiRoute =
    new ErgoPeersApiRoute(peerManager, networkController, syncTracker, timeProvider, settings)(context, ec)

}
