package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.Future

case class InfoRoute(statsCollector: ActorRef,
                     override val settings: RESTApiSettings,
                     timeProvider: NetworkTimeProvider)
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = info

  private def getMinerInfo: Future[MiningStatusResponse] = (statsCollector ? MiningStatusRequest).mapTo[MiningStatusResponse]

  def info: Route = (path("info") & get) {
    val timeJson = Map("currentTime" -> timeProvider.time().asJson).asJson
    (statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(_.asJson.deepMerge(timeJson)).okJson()
  }
}
