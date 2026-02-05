package org.ergoplatform.http.routes

import java.net.InetSocketAddress
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{P2PKAddress, Pay2SAddress, Pay2SHAddress}
import org.ergoplatform.http.api.ErgoUtilsApiRoute
import org.ergoplatform.settings.RESTApiSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigma.serialization.ErgoTreeSerializer

import scala.concurrent.duration._

class UtilsApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  val prefix = "/utils"

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)
  val route: Route = ErgoUtilsApiRoute(digestReadersRef, settings).route
  val p2pkaddress = P2PKAddress(defaultMinerPk)(settings.addressEncoder)
  val p2shaddress = Pay2SHAddress(feeProp)(settings.addressEncoder)
  val p2saddress = Pay2SAddress(feeProp)(settings.addressEncoder)

  val treeSerializer: ErgoTreeSerializer = new ErgoTreeSerializer

  it should "derive address from ErgoTree (p2s)" in {
    val et = Base16.encode(treeSerializer.serializeErgoTree(p2saddress.script))
    Get(s"$prefix/ergoTreeToAddress/$et") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(p2saddress.toString())
    }
  }

  it should "validate correct p2s address" in {
    Get(s"$prefix/address/$p2saddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received node info: $json")
      val c = json.hcursor
      c.downField("address").as[String] shouldEqual Right(p2saddress.toString())
      c.downField("isValid").as[Boolean] shouldEqual Right(true)
    }
  }

  it should "validate incorrect address (p2s)" in {
    val invalidAddress = p2saddress + "aa"
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

  //p2sh

  it should "derive address from ErgoTree (p2sh)" in {
    val et = Base16.encode(treeSerializer.serializeErgoTree(p2shaddress.script))
    Get(s"$prefix/ergoTreeToAddress/$et") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(p2shaddress.toString())
    }
  }

  it should "validate correct p2sh address" in {
    Get(s"$prefix/address/$p2shaddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received node info: $json")
      val c = json.hcursor
      c.downField("address").as[String] shouldEqual Right(p2shaddress.toString())
      c.downField("isValid").as[Boolean] shouldEqual Right(true)
    }
  }

  it should "validate incorrect address (p2sh)" in {
    val invalidAddress = p2shaddress + "aa"
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

  //p2pk

  it should "do correct raw/address roundtrip (p2pk)" in {
    var raw: String = null

    Get(s"$prefix/addressToRaw/$p2pkaddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      val c = json.hcursor
      raw = c.downField("raw").as[String].toOption.get
    }

    Get(s"$prefix/rawToAddress/$raw") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("address").as[String] shouldEqual Right(p2pkaddress.toString())
    }
  }

  it should "derive address from ErgoTree (p2pk)" in {
    val et = Base16.encode(treeSerializer.serializeErgoTree(p2pkaddress.script))
    Get(s"$prefix/ergoTreeToAddress/$et") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(p2pkaddress.toString())
    }
  }

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

  it should "validate incorrect address (p2pk)" in {
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

  it should "return error for schnorrSign when wallet is not initialized" in {
    val requestJson = Json.obj(
      "signerAddress" -> p2pkaddress.toString.asJson,
      "message" -> "02415748f8eef16c5ea6896cec3a8defccc8a0dace245248be66ffd6ff2159da32000000000003d09000000000694fa26d".asJson
    )

    Post(s"$prefix/schnorrSign", requestJson) ~> route ~> check {
      status shouldBe StatusCodes.InternalServerError
      val response = responseAs[Json]
      println(s"SchnorrSign response: $response")
    }
  }

  it should "return error for schnorrSign with non-P2PK address" in {
    val requestJson = Json.obj(
      "signerAddress" -> p2shaddress.toString.asJson,
      "message" -> "02415748f8eef16c5ea6896cec3a8defccc8a0dace245248be66ffd6ff2159da32000000000003d09000000000694fa26d".asJson
    )

    Post(s"$prefix/schnorrSign", requestJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      println(responseAs[Json])
      val response = responseAs[Json]
      response.hcursor.downField("detail").as[String] shouldEqual Right("InvalidAddressType")
    }
  }

  it should "return error for schnorrSign with invalid hex message" in {
    val requestJson = Json.obj(
      "signerAddress" -> p2pkaddress.toString.asJson,
      "message" -> "invalid_hex_message".asJson
    )

    Post(s"$prefix/schnorrSign", requestJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      val response = responseAs[Json]
      response.hcursor.downField("detail").as[String] shouldEqual Right("InvalidMessage")
    }
  }

  it should "return error for schnorrSign with invalid address" in {
    val requestJson = Json.obj(
      "signerAddress" -> "invalid_address".asJson,
      "message" -> "02415748f8eef16c5ea6896cec3a8defccc8a0dace245248be66ffd6ff2159da32000000000003d09000000000694fa26d".asJson
    )

    Post(s"$prefix/schnorrSign", requestJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      val response = responseAs[Json]
      response.hcursor.downField("detail").as[String] shouldEqual Right("InvalidAddress")
    }
  }

}

