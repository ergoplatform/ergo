package org.ergoplatform.http.api

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import io.circe.Encoder
import org.ergoplatform._
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.scanning.{EqualsScanningPredicate, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings}
import scorex.core.api.http.ApiResponse

import scala.util.{Failure, Success}
import ScanEntities._
import org.ergoplatform.ErgoBox.R1
import org.ergoplatform.http.api.ApiError.BadRequest
import org.ergoplatform.wallet.Constants.ScanId
import sigmastate.Values.ByteArrayConstant
import sigmastate.serialization.ErgoTreeSerializer

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

  import org.ergoplatform.nodeView.wallet.scanning.ScanJsonCodecs.{scanReqDecoder, scanEncoder}

  implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val walletBoxEncoder: Encoder[WalletBox] = WalletBox.encoder

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  override val route: Route = (pathPrefix("scan") & withAuth) {
    registerR ~
      deregisterR ~
      listScansR ~
      unspentR ~
      spentR ~
      stopTrackingR ~
      addBoxR ~
      p2sRuleR
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

  def unspentR: Route = (path("unspentBoxes" / IntNumber) & get & boxParams) {
    (scanIdInt, minConfNum, maxConfNum, minHeight, maxHeight) =>
      val scanId = ScanId @@ scanIdInt.toShort
      val considerUnconfirmed = minConfNum == -1
      withWallet(_.scanUnspentBoxes(scanId, considerUnconfirmed, minHeight, maxHeight).map {
        _.filter(boxConfirmationFilter(_, minConfNum, maxConfNum))
      })
  }

  def spentR: Route = (path("spentBoxes" / IntNumber) & get & boxParams) {
    (scanIdInt, minConfNum, maxConfNum, minHeight, maxHeight) =>
      val scanId = ScanId @@ scanIdInt.toShort
      withWallet(_.scanSpentBoxes(scanId).map {
        _.filter(boxConfirmationHeightFilter(_, minConfNum, maxConfNum, minHeight, maxHeight))
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

  /**
    * API method to get tracking rule corresponding to p2s address
    */
  def p2sRuleR: Route = (path("p2sRule") & post & entity(as[String])) { p2sRaw =>
    val p2s = fromJsonOrPlain(p2sRaw)
    addressEncoder.fromString(p2s) match {
      case Success(p2sAddr) =>
        val script = p2sAddr.script
        val scriptBytes = ByteArrayConstant(ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(script))
        val trackingRule = EqualsScanningPredicate(R1, scriptBytes)
        val request = ScanRequest(p2s, trackingRule, Some(ScanWalletInteraction.Off), Some(true))
        withWalletOp(_.addScan(request).map(_.response)) {
          case Failure(e) => BadRequest(s"Bad request $request. ${Option(e.getMessage).getOrElse(e.toString)}")
          case Success(app) => ApiResponse(ScanIdWrapper(app.scanId))
        }
      case Failure(e) => BadRequest(s"Can't parse $p2s. ${Option(e.getMessage).getOrElse(e.toString)}")
    }
  }
}


