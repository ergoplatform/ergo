package org.ergoplatform.api

import io.circe.syntax._
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.Json
import org.ergoplatform.http.api.{ApiCodecs, ErgoBaseApiRoute}
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplicationStorage}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.util.{Failure, Success}


final case class ApplicationApiRoute (ergoSettings: ErgoSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ExternalApplicationJsonCodecs._

  lazy val storage = ExternalApplicationStorage.readOrCreate(ergoSettings)

  override val route: Route = (pathPrefix("application") & withAuth) {
    registerR ~
      deregisterR ~
      listR
  }

  def deregisterR: Route = (path("deregister" / LongNumber) & get) {id =>
    storage.removeApplication(id)
    ApiResponse(Json.obj("id" -> id.asJson))
  }

  def registerR: Route = (path("register") & post
    & entity(as[ExternalAppRequest])) { request =>

    storage.addApplication(request) match {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) =>
        ApiResponse(Json.obj("id" -> app.appId.asJson))
    }
  }

  def listR: Route = (path("listAll") & get) {
    ApiResponse(storage.allApplications)
  }

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi
}