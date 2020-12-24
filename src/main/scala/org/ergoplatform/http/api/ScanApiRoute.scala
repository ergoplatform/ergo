package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.{Directive, Route}
import io.circe.Encoder
import org.ergoplatform._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.scanning.ScanRequest
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.ApiResponse
import scorex.core.settings.RESTApiSettings
import scala.util.{Failure, Success}
import ScanEntities._
import org.ergoplatform.wallet.Constants.ScanId

/**
  * This class contains methods to register / deregister and list external scans, and also to serve them.
  * For serving external scans, this class has following methods:
  *   * methods to track or stop tracking some box
  *   * a method to list boxes not spent yet
  *
  * See EIP-0001 (https://github.com/ergoplatform/eips/blob/master/eip-0001.md) for motivation behind this API.
  */
case class ScanApiRoute(readersHolder: ActorRef, ergoSettings: ErgoSettings)
                       (implicit val context: ActorRefFactory) extends WalletApiOperations with ApiCodecs {

  import org.ergoplatform.nodeView.wallet.scanning.ScanJsonCodecs._

  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val walletBoxEncoder: Encoder[WalletBox] = WalletBox.encoder

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("scan") & withAuth) {
    registerR ~
      deregisterR ~
      listScansR ~
      unspentR ~
      stopTrackingR ~
      addBoxR
  }

  def registerR: Route = (path("register") & post & entity(as[ScanRequest])) { request =>
    withWalletOp(_.addScan(request).map(_.response)) {
      case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(app) => ApiResponse(ScanIdWrapper(app.scanId))
    }
  }

  def deregisterR: Route = (path("deregister") & post & entity(as[ScanIdWrapper])) { scanId =>
    withWalletOp(_.removeScan(scanId.scanId).map(_.response)) {
      case Failure(e) => BadRequest(s"No scan exists or db error: ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(ScanIdWrapper(scanId.scanId))
    }
  }

  def listScansR: Route = (path("listAll") & get) {
    withWallet(_.readScans().map(_.apps))
  }

  def unspentR: Route = (path("unspentBoxes" / IntNumber) & get & boxParams) { (scanIdInt, minConfNum, minHeight) =>
    val scanId = ScanId @@ scanIdInt.toShort
    val considerUnconfirmed = minConfNum == -1
    withWallet(_.appBoxes(scanId, unspentOnly = true, considerUnconfirmed).map {
      _.filter(boxFilterPredicate(_, minConfNum, minHeight))
    })
  }

  def stopTrackingR: Route = (path("stopTracking") & post & entity(as[ScanIdBoxId])) { scanIdBoxId =>
    withWalletOp(_.stopTracking(scanIdBoxId.scanId, scanIdBoxId.boxId).map(_.status)) {
      case Failure(e) => BadRequest(s"Bad request ($scanIdBoxId): ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(scanIdBoxId)
    }
  }

  def addBoxR: Route = (path("addBox") & post & entity(as[BoxWithScanIds])) { scanIdsBox =>
    withWalletOp(_.addBox(scanIdsBox.box, scanIdsBox.scanIds).map(_.status)) {
      case Failure(e) => BadRequest(s"Bad request ($scanIdsBox): ${Option(e.getMessage).getOrElse(e.toString)}")
      case Success(_) => ApiResponse(scanIdsBox.box.id)
    }
  }
}
