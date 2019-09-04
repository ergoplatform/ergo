package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import org.ergoplatform.nodeView.wallet.scanning.ExternalAppRequest
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.util.{Failure, Success}


class ApplicationApiRoute (readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ExternalApplicationJsonCodecs._

  override val route: Route = (pathPrefix("wallet") & withAuth) {
    registerR
  }

  def registerR: Route = (path("register") & post
    & entity(as[ExternalAppRequest])) { request =>
    val appId = 2
    request.toApp(appId) match {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) =>
        ApiResponse(appId)
    }
  }

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi
}