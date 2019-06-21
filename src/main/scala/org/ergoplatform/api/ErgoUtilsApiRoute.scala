package org.ergoplatform.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.settings.ErgoSettings
import scorex.core.api.http.{ApiResponse, UtilsApiRoute}

import scala.util.Failure

class ErgoUtilsApiRoute(ergoSettings: ErgoSettings)(implicit context: ActorRefFactory) extends UtilsApiRoute(ergoSettings.scorexSettings.restApi)(context) {

  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

  override val route: Route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b ~ address
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