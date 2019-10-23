package org.ergoplatform.api

import io.circe.syntax._
import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import org.ergoplatform.http.api.{ApiCodecs, ErgoBaseApiRoute}
import org.ergoplatform.nodeView.wallet.persistence.WalletStorage
import org.ergoplatform.nodeView.wallet.scanning.ExternalAppRequest
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.util.{Failure, Success}

/**
  * This class contains methods to register / deregister and list external applications.
  * See EIP-0001 (https://github.com/ergoplatform/eips/blob/master/eip-0001.md)
  */

final case class ApplicationApiRoute (ergoSettings: ErgoSettings)
                          (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ExternalApplicationJsonCodecs._

  lazy val storage: WalletStorage = ergoSettings.walletStorage

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private def encodeId(id: Short): Json = Json.obj("id" -> id.asJson)

  override val route: Route = (pathPrefix("application") & withAuth) {
    registerR ~
      deregisterR ~
      listR
  }

  def deregisterR: Route = (path("deregister" / IntNumber) & get) {idInt =>
    val id = idInt.toShort
    storage.removeApplication(id)
    ApiResponse(encodeId(id))
  }

  def registerR: Route = (path("register") & post & entity(as[ExternalAppRequest])) { request =>
    storage.addApplication(request) match {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(encodeId(app.appId))
    }
  }

  def listR: Route = (path("listAll") & get) {
    ApiResponse(storage.allApplications)
  }

}