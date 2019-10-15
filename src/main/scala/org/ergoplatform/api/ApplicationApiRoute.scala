package org.ergoplatform.api

import io.circe.syntax._
import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.db.LDBFactory
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

  //todo: move this storage out of this class during further steps of EIP-1 implementation
  lazy val storage = new WalletStorage(LDBFactory.createKvDb(s"${ergoSettings.directory}/apps"), ergoSettings)(new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix))

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private def encodeId(id: Long) = Json.obj("id" -> id.asJson)

  override val route: Route = (pathPrefix("application") & withAuth) {
    registerR ~
      deregisterR ~
      listR
  }

  def deregisterR: Route = (path("deregister" / LongNumber) & get) {id =>
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