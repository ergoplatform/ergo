package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.Future

case class InfoRoute(statsCollector: ActorRef,
                     override val settings: RESTApiSettings)
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = info

  private def getMinerInfo: Future[MiningStatusResponse] = (statsCollector ? MiningStatusRequest).mapTo[MiningStatusResponse]

  def info: Route = (path("info") & get) {
    (statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(_.json).okJson()
  }
}
