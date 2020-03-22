package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.Encoder
import org.ergoplatform._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.scanning.ExternalAppRequest
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings

import scala.util.{Failure, Success}
import ApplicationEntities._
import org.ergoplatform.wallet.Constants.ApplicationId

/**
  * This class contains methods to register / deregister and list external applications, and also to serve them.
  * For serving external applications, this class has following methods:
  *   * list boxes which can belong to an application but that is not certain
  *   * a method to mark a box as certainly belongs to an application
  *   * a method to stop tracking some box
  *   * a method to list certain boxes not spent yet
  * Please note that there's a flag which makes all the new boxes certain by default.
  *
  * See EIP-0001 (https://github.com/ergoplatform/eips/blob/master/eip-0001.md) for motivation behind this API.
  */

case class ApplicationApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                              (implicit val context: ActorRefFactory) extends WalletApiOperations with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ExternalApplicationJsonCodecs._

  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val walletBoxEncoder: Encoder[WalletBox] = WalletBox.encoder

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("application") & withAuth) {
    registerR ~
      deregisterR ~
      listR ~
      uncertainR ~
      unspentR ~
      makeCertainR ~
      stopTrackingR
  }

  def deregisterR: Route = (path("deregister") & post & entity(as[ApplicationIdWrapper])) { appId =>
    withWalletOp(_.removeApplication(appId.appId).map(_.response)) {
      case Failure(e) => BadRequest(s"No application exists or db error: ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(ApplicationIdWrapper(appId.appId))
    }
  }

  def registerR: Route = (path("register") & post & entity(as[ExternalAppRequest])) { request =>
    withWalletOp(_.addApplication(request).map(_.response)) {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(ApplicationIdWrapper(app.appId))
    }
  }

  //todo: paging?
  def listR: Route = (path("listAll") & get) {
    withWallet(_.readApplications())
  }

  def uncertainR: Route = (path("uncertainBoxes" / IntNumber) & get & boxParams) { (appIdInt, minConfNum, minHeight) =>
    val appId = ApplicationId @@ appIdInt.toShort
    withWallet(_.uncertainBoxes(appId).map {
      _.filter(boxPredicate(_, minConfNum, minHeight))
    })
  }

  def unspentR: Route = (path("unspentBoxes" / IntNumber) & get & boxParams) { (appIdInt, minConfNum, minHeight) =>
    val appId = ApplicationId @@ appIdInt.toShort
    withWallet(_.appBoxes(appId, unspentOnly = true).map {
      _.filter(boxPredicate(_, minConfNum, minHeight))
    })
  }

  def makeCertainR: Route = (path("makeCertain") & post & entity(as[ApplicationIdBoxId])) { appIdBoxId =>
    withWalletOp(_.makeCertain(appIdBoxId.appId, appIdBoxId.boxId)) {
      case Failure(e) => BadRequest(s"Bad request ($appIdBoxId): ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(appIdBoxId)
    }
  }

  def stopTrackingR: Route = (path("stopTracking") & post & entity(as[ApplicationIdBoxId])) { appIdBoxId =>
    withWalletOp(_.stopTracking(appIdBoxId.appId, appIdBoxId.boxId)) {
      case Failure(e) => BadRequest(s"Bad request ($appIdBoxId): ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(appIdBoxId)
    }
  }

}