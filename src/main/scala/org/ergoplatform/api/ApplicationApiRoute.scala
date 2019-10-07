package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import org.ergoplatform.db.LDBFactory
import org.ergoplatform.http.api.{ApiCodecs, ErgoBaseApiRoute}
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplicationStorage}
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.util.{Failure, Success}


final case class ApplicationApiRoute (readersHolder: ActorRef, nodeViewActorRef: ActorRef, ergoSettings: ErgoSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ExternalApplicationJsonCodecs._

  val storage = new ExternalApplicationStorage(LDBFactory.createKvDb("/tmp/1"))

  override val route: Route = (pathPrefix("application")) { //& withAuth) {
    registerR ~
      deregisterR ~
      listR
  }

  def deregisterR: Route = (path("deregister" / LongNumber) & get) {id =>
    storage.removeApplication(id)
    ApiResponse(id)
  }

  def registerR: Route = (path("register") & post
    & entity(as[ExternalAppRequest])) { request =>

    storage.addApplication(request) match {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) =>
        ApiResponse(app.appId)
    }
  }

  def listR: Route = (path("list") & get) {
    ApiResponse(storage.allApplications)
  }

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi
}