package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.NetworkTimeProvider


case class InfoApiRoute(statsCollector: ActorRef,
                        settings: RESTApiSettings,
                        timeProvider: NetworkTimeProvider)
                       (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  override val route: Route = (path("info") & get) {
      val timeJson = Map("currentTime" -> timeProvider.time().asJson).asJson
      ApiResponse((statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(_.asJson.deepMerge(timeJson)))
    }
  }

