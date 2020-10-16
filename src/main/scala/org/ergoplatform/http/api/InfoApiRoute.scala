package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiResponse
import scorex.core.utils.NetworkTimeProvider


case class InfoApiRoute(statsCollector: ActorRef,
                        ergoSettings: ErgoSettings,
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  override val settings = ergoSettings.scorexSettings.restApi

  override val route: Route = (path("info") & get) {
    val now = timeProvider.time()
    ApiResponse((statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(ni => ni.asJson.deepMerge {
      val maxDelta = ergoSettings.scorexSettings.network.syncStatusRefreshStable.toMillis
      val connected = now < (ni.lastIncomingMessageTime + maxDelta)
      Map("currentTime" -> now.asJson, "connected" -> connected.asJson).asJson
    }))
  }

}
