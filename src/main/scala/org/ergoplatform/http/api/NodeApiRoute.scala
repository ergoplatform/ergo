package org.ergoplatform.http.api

import akka.actor.{ActorRefFactory, ActorSystem}
import akka.http.scaladsl.server.Route
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scala.concurrent.duration._

case class NodeApiRoute(ergoSettings: ErgoSettings)(implicit system: ActorSystem, val context: ActorRefFactory) extends ErgoBaseApiRoute {

  val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("node") & withAuth) {
      shutdown
    }

  private val shutdownDelay = 5

  private def shutdown: Route = (pathPrefix("shutdown") & post) {
    system.scheduler.scheduleOnce(shutdownDelay.seconds)(system.terminate())
    ApiResponse(s"Shutting down in $shutdownDelay seconds")
  }
}
