package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.NetworkTimeProvider

case class InfoRoute(statsCollector: ActorRef,
                     override val settings: RESTApiSettings,
                     timeProvider: NetworkTimeProvider)
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = info

  def info: Route = (path("info") & get) {
    val timeJson = Map("currentTime" -> timeProvider.time().asJson).asJson
    (statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(_.asJson.deepMerge(timeJson)).okJson()
  }
}
