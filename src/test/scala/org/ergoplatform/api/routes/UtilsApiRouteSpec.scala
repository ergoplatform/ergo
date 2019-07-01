package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.api.ErgoUtilsApiRoute
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

class UtilsApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  val prefix = "/utils"

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds)
  val route: Route = ErgoUtilsApiRoute(settings).route
  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(settings.chainSettings.addressPrefix)
  val p2pkaddress = P2PKAddress(defaultMinerPk)

  it should "validate correct p2pk address" in {
    Get(s"$prefix/address/$p2pkaddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received node info: $json")
      val c = json.hcursor
      c.downField("address").as[String] shouldEqual Right(p2pkaddress.toString())
      c.downField("isValid").as[Boolean] shouldEqual Right(true)
    }
  }

  it should "validate incorrect address" in {
    val invalidAddress = p2pkaddress + "aa"
    Get(s"$prefix/address/$invalidAddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received node info: $json")
      val c = json.hcursor
      c.downField("address").as[String] shouldEqual Right(invalidAddress)
      c.downField("isValid").as[Boolean] shouldEqual Right(false)
      c.downField("error").as[String] shouldEqual Right("requirement failed: Trying to decode mainnet address in testnet")
    }
  }

}

