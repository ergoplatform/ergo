package org.ergoplatform.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform._
import org.ergoplatform.http.api.{ApiCodecs, WalletApiOperations}
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.scanning.ExternalAppRequest
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.util.{Failure, Success}
import scorex.crypto.authds.ADKey

/**
  * This class contains methods to register / deregister and list external applications.
  * See EIP-0001 (https://github.com/ergoplatform/eips/blob/master/eip-0001.md)
  */

final case class ApplicationApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                          (implicit val context: ActorRefFactory) extends WalletApiOperations with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ExternalApplicationJsonCodecs._

  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val walletBoxEncoder: Encoder[WalletBox] = WalletBox.encoder

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private def encodeAppId(id: Short): Json = Json.obj("id" -> id.asJson)

  override val route: Route = (pathPrefix("application") & withAuth) {
    registerR ~
      deregisterR ~
      listR ~
      uncertainR ~
      unspentR ~
      makeCertainR ~
      stopTrackingR
  }

  def deregisterR: Route = (path("deregister" / IntNumber) & get) {idInt =>
    val id = idInt.toShort
    withWalletOp(_.removeApplication(id)) {
      case Failure(e) => BadRequest(s"No application exists or db error: ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(encodeAppId(id))
    }
  }

  def registerR: Route = (path("register") & post & entity(as[ExternalAppRequest])) { request =>
    withWalletOp(_.addApplication(request)) {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(encodeAppId(app.appId))
    }
  }

  //todo: paging
  def listR: Route = (path("listAll") & get) {
    withWallet(_.readApplications())
  }

  //todo: paging
  def uncertainR: Route = (path("uncertainBoxes" / IntNumber) & get) {appIdInt =>
    val appId = appIdInt.toShort
    withWallet(_.uncertainBoxes(appId))
  }

  //todo: paging
  def unspentR: Route = (path("unspentBoxes" / IntNumber) & get) {appIdInt =>
    val appId = appIdInt.toShort
    withWallet(_.appBoxes(appId, unspentOnly = true))
  }

  def makeCertainR: Route = (path("makeCertain" / IntNumber) & modifierIdGet & get) {case (appIdInt, boxId) =>
    val appId = appIdInt.toShort
    withWalletOp(_.makeCertain(appId, ADKey @@ Algos.decode(boxId).get)) {
      case Failure(e) => BadRequest(s"Bad request ($appIdInt, $boxId): ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(encodeAppId(appId))
    }
  }

  def stopTrackingR: Route = (path("stopTracking" / IntNumber) & modifierIdGet & get) {case (appIdInt, boxId) =>
    val appId = appIdInt.toShort
    withWalletOp(_.stopTracking(appId, ADKey @@ Algos.decode(boxId).get)) {
      case Failure(e) => BadRequest(s"Bad request ($appIdInt, $boxId): ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(encodeAppId(appId))
    }
  }


  //todo: add box

}