package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.scanning.ExternalAppRequest
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey

import scala.util.{Failure, Success}

/**
  * This class contains methods to register / deregister and list external applications.
  * See EIP-0001 (https://github.com/ergoplatform/eips/blob/master/eip-0001.md)
  */


case class ApplicationId(appId: AppId)

object ApplicationId {

  implicit val applicationIdEncoder: Encoder[ApplicationId] = { appStatus =>
    Json.obj("appId" -> appStatus.appId.asJson)
  }

  implicit val applicationIdDecoder: Decoder[ApplicationId] = { c: HCursor =>
    for {
      appId <- c.downField("appId").as[Short]
    } yield ApplicationId(appId)
  }

}

case class ApplicationIdBoxId(appId: AppId, boxId: BoxId)

object ApplicationIdBoxId extends JsonCodecs {

  implicit val applicationIdBoxIdEncoder: Encoder[ApplicationIdBoxId] = { appStatus =>
    Json.obj("appId" -> appStatus.appId.asJson, "boxId" -> Algos.encode(appStatus.boxId).asJson)
  }

  implicit val applicationIdDecoder: Decoder[ApplicationIdBoxId] = { c: HCursor =>
    for {
      appId <- c.downField("appId").as[Short]
      boxId <- c.downField("boxId").as[ADKey]
    } yield ApplicationIdBoxId(appId, boxId)
  }

}

case class ApplicationIdBox(appId: AppId, boxBytes: Array[Byte])

object ApplicationIdBox extends JsonCodecs {

  implicit val applicationIdDecoder: Decoder[ApplicationIdBox] = { c: HCursor =>
    for {
      appId <- c.downField("appId").as[Short]
      boxBytes <- c.downField("boxBytes").as[Array[Byte]]
    } yield ApplicationIdBox(appId, boxBytes)
  }

}


final case class ApplicationApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
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

  def deregisterR: Route = (path("deregister") & post & entity(as[ApplicationId])) { appId =>
    withWalletOp(_.removeApplication(appId.appId)) {
      case Failure(e) => BadRequest(s"No application exists or db error: ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(appId.asJson)
    }
  }

  def registerR: Route = (path("register") & post & entity(as[ExternalAppRequest])) { request =>
    withWalletOp(_.addApplication(request)) {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(ApplicationId(app.appId))
    }
  }

  //todo: paging
  def listR: Route = (path("listAll") & get) {
    withWallet(_.readApplications())
  }

  //todo: paging
  def uncertainR: Route = (path("uncertainBoxes" / IntNumber) & get) { appIdInt =>
    val appId = appIdInt.toShort
    withWallet(_.uncertainBoxes(appId))
  }

  //todo: paging
  def unspentR: Route = (path("unspentBoxes" / IntNumber) & get) { appIdInt =>
    val appId = appIdInt.toShort
    withWallet(_.appBoxes(appId, unspentOnly = true))
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