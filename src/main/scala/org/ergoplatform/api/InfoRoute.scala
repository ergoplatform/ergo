package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, GetParameters, NodeInfo}
import org.ergoplatform.settings.Parameters
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.NetworkTimeProvider

import org.ergoplatform.settings.ParametersSerializer.jsonEncoder

case class InfoRoute(statsCollector: ActorRef,
                     settings: RESTApiSettings,
                     timeProvider: NetworkTimeProvider)
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route: Route = withCors {
    info ~ params
  }

  def info: Route = (path("info") & get) {
    val timeJson = Map("currentTime" -> timeProvider.time().asJson).asJson
    ApiResponse((statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(_.asJson.deepMerge(timeJson)))
  }

  def params: Route = (path("parameters") & get) {
    val timeJson = Map("currentTime" -> timeProvider.time().asJson).asJson
    ApiResponse((statsCollector ? GetParameters).mapTo[Parameters].map(_.asJson.deepMerge(timeJson)))
  }
}
