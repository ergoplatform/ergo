package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.{ApiResponse, UtilsApiRoute}
import scorex.util.encode.Base16
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer}

import scala.util.Failure

class ErgoUtilsApiRoute(ergoSettings: ErgoSettings)(implicit context: ActorRefFactory) extends UtilsApiRoute(ergoSettings.scorexSettings.restApi)(context) {

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

  override val route: Route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b ~ address ~ rawToAddress ~ addressToRaw
  }

  def rawToAddress: Route = (get & path("rawToAddress" / Segment)) { pubKeyHex =>
    Base16.decode(pubKeyHex)
      .flatMap(pkBytes => GroupElementSerializer.parseTry(SigmaSerializer.startReader(pkBytes)))
      .map(pkPoint => P2PKAddress(ProveDlog(pkPoint)))
      .fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(Map("address" -> address.toString().asJson).asJson)
      )
  }

  def addressToRaw: Route = (get & path("addressToRaw" / Segment)) { addressStr =>
    ergoAddressEncoder.fromString(addressStr)
        .map(address => address.contentBytes)
        .map(Base16.encode)
        .fold(
          e => BadRequest(e.getMessage),
          raw => ApiResponse(Map("raw" -> raw).asJson)
        )
  }

  def address: Route = (get & path("address" / Segment)) { addressStr =>
    val address = ergoAddressEncoder.fromString(addressStr)
    val error = address match {
      case Failure(exception) => Map("error" -> exception.getMessage.asJson)
      case _ => Map()
    }

    val resp: Map[String, Json] = error ++ Map(
      "address" -> addressStr.asJson,
      "isValid" -> address.isSuccess.asJson
    )

    ApiResponse(resp.asJson)
  }

}

object ErgoUtilsApiRoute {

  def apply(ergoSettings: ErgoSettings)(implicit context: ActorRefFactory): ErgoUtilsApiRoute = {
    new ErgoUtilsApiRoute(ergoSettings)
  }

}