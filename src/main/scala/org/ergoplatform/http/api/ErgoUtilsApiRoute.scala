package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.core.api.http.ApiError.BadRequest
import scorex.core.api.http.{ApiError, ApiResponse, ApiRoute}
import scorex.core.settings.RESTApiSettings
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.basics.DLogProtocol.ProveDlog
import java.security.SecureRandom
import scala.util.Failure
import sigmastate.serialization.{
  ErgoTreeSerializer,
  GroupElementSerializer,
  SigmaSerializer
}

class ErgoUtilsApiRoute(val ergoSettings: ErgoSettings)(
  implicit val context: ActorRefFactory
) extends ApiRoute
  with ScorexEncoding {

  private val SeedSize = 32
  private val treeSerializer: ErgoTreeSerializer = new ErgoTreeSerializer

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  implicit val ergoAddressEncoder: ErgoAddressEncoder =
    new ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

  override val route: Route = pathPrefix("utils") {
    seedRoute ~
    length ~
    hashBlake2b ~
    addressR ~
    rawToAddressR ~
    addressToRawR ~
    ergoTreeToAddressR
  }

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    encoder.encode(seed)
  }

  def seedRoute: Route = (get & path("seed")) {
    ApiResponse(seed(SeedSize))
  }

  def length: Route = (get & path("seed" / IntNumber)) { length =>
    ApiResponse(seed(length))
  }

  def hashBlake2b: Route = {
    (post & path("hash" / "blake2b") & entity(as[Json])) { json =>
      json.asString match {
        case Some(message) => ApiResponse(encoder.encode(Blake2b256(message)))
        case None          => ApiError.BadRequest
      }
    }
  }

  def rawToAddressR: Route = (get & path("rawToAddress" / Segment)) { pubKeyHex =>
    Base16
      .decode(pubKeyHex)
      .flatMap(pkBytes =>
        GroupElementSerializer.parseTry(SigmaSerializer.startReader(pkBytes))
      )
      .map(pkPoint => P2PKAddress(ProveDlog(pkPoint)))
      .fold(
        e => BadRequest(e.getMessage),
        address => ApiResponse(Map("address" -> address.toString().asJson).asJson)
      )
  }

  def addressToRawR: Route = (get & path("addressToRaw" / Segment)) { addressStr =>
    ergoAddressEncoder
      .fromString(addressStr)
      .map(address => address.contentBytes)
      .map(Base16.encode)
      .fold(
        e => BadRequest(e.getMessage),
        raw => ApiResponse(Map("raw" -> raw).asJson)
      )
  }

  def ergoTreeToAddressR: Route = (get & path("ergoTreeToAddress" / Segment)) {
    ergoTreeHex =>
      Base16
        .decode(ergoTreeHex)
        .flatMap { etBytes =>
          ergoAddressEncoder.fromProposition(treeSerializer.deserializeErgoTree(etBytes))
        }
        .fold(
          e => BadRequest(e.getMessage),
          address => ApiResponse(Map("address" -> address.toString.asJson).asJson)
        )
  }

  def addressR: Route = (get & path("address" / Segment)) { addressStr =>
    val address = ergoAddressEncoder.fromString(addressStr)
    val error = address match {
      case Failure(exception) => Map("error" -> exception.getMessage.asJson)
      case _                  => Map()
    }

    val resp: Map[String, Json] = error ++ Map(
        "address" -> addressStr.asJson,
        "isValid" -> address.isSuccess.asJson
      )

    ApiResponse(resp.asJson)
  }

}

object ErgoUtilsApiRoute {

  def apply(
    ergoSettings: ErgoSettings
  )(implicit context: ActorRefFactory): ErgoUtilsApiRoute = {
    new ErgoUtilsApiRoute(ergoSettings)
  }

}
