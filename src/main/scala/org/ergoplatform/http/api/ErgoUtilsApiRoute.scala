package org.ergoplatform.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.ApiError.BadRequest
import org.ergoplatform.settings.{ErgoSettings, RESTApiSettings}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.core.api.http.{ApiResponse, ApiRoute}
import org.ergoplatform.utils.ScorexEncoding
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigma.data.ProveDlog

import java.security.SecureRandom
import scala.util.Failure
import sigma.serialization.{ErgoTreeSerializer, GroupElementSerializer, SigmaSerializer}

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
    rawToAddressPostR ~
    rawToAddressR ~
    addressToRawPostR ~
    addressToRawR ~
    validateAddressPostR ~
    validateAddressGetR ~
    ergoTreeToAddressPostR ~
    ergoTreeToAddressGetR
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
      json.as[String] match {
        case Right(message) => ApiResponse(encoder.encode(Blake2b256(message)))
        case Left(ex)       => ApiError(StatusCodes.BadRequest, ex.getMessage())
      }
    }
  }

  private def rawToAddressResponse(pubKeyHex: String) = {
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

  def rawToAddressGetR: Route = (get & path("rawToAddress" / Segment))(rawToAddressResponse)
  def rawToAddressR: Route = rawToAddressGetR

  def rawToAddressPostR: Route = (post & path("rawToAddress") & entity(as[Json])) { json =>
    json.as[String] match {
      case Right(pubKeyHex) => rawToAddressResponse(pubKeyHex)
      case Left(ex)         => ApiError(StatusCodes.BadRequest, ex.getMessage())
    }
  }

  private def addressToRawResponse(addressStr: String) = {
    ergoAddressEncoder
      .fromString(addressStr)
      .map(address => address.contentBytes)
      .map(Base16.encode)
      .fold(
        e => BadRequest(e.getMessage),
        raw => ApiResponse(Map("raw" -> raw).asJson)
      )
  }

  def addressToRawGetR: Route = (get & path("addressToRaw" / Segment))(addressToRawResponse)
  def addressToRawR: Route = addressToRawGetR

  def addressToRawPostR: Route = (post & path("addressToRaw") & entity(as[Json])) { json =>
    json.as[String] match {
      case Right(addressStr) => addressToRawResponse(addressStr)
      case Left(ex)          => ApiError(StatusCodes.BadRequest, ex.getMessage())
    }
  }

  private def ergoTreeToAddressResponse(ergoTreeHex: String) = {
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

  def ergoTreeToAddressGetR: Route = (get & path("ergoTreeToAddress" / Segment))(ergoTreeToAddressResponse)
  def ergoTreeToAddressPostR: Route = (post & path("ergoTreeToAddress") & entity(as[Json])) { json =>
    json.as[String] match {
      case Right(ergoTreeHex) => ergoTreeToAddressResponse(ergoTreeHex)
      case Left(ex)           => ApiError(StatusCodes.BadRequest, ex.getMessage())
    }
  }

  private def validateAddressResponse(addressStr: String) = {
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

  def validateAddressGetR: Route = (get & path("address" / Segment))(validateAddressResponse)

  def validateAddressPostR: Route = (post & path("address") & entity(as[Json])) { json =>
    json.as[String] match {
      case Right(addressStr) => validateAddressResponse(addressStr)
      case Left(ex)          => ApiError(StatusCodes.BadRequest, ex.getMessage())
    }
  }
}

object ErgoUtilsApiRoute {

  def apply(
    ergoSettings: ErgoSettings
  )(implicit context: ActorRefFactory): ErgoUtilsApiRoute = {
    new ErgoUtilsApiRoute(ergoSettings)
  }

}
