package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import org.ergoplatform.settings.RESTApiSettings
import scorex.core.api.http.ApiResponse


/**
  * API methods corresponding to /info path
  */
case class InfoApiRoute(statsCollector: ActorRef,
                        settings: RESTApiSettings)
                       (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {

  override val route: Route = {
    (path("info") & get) {
      val timeJson = Map("currentTime" -> System.currentTimeMillis().asJson).asJson
      ApiResponse((statsCollector ? GetNodeInfo).mapTo[NodeInfo].map(_.asJson.deepMerge(timeJson)))
    } ~
    (path(".well-known" / "ai-plugin.json") & get) {
      getFromResource(".well-known/ai-plugin.json", ContentTypes.`application/json`)
    } ~
    (path("openapi.yaml") & get) {
      getFromResource("api/openapi-ai.yaml", ContentTypes.`text/plain(UTF-8)`)
    }
  }

}
