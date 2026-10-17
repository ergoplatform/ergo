package org.ergoplatform.http.routes

import java.net.InetSocketAddress
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
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
import sigma.ast.{ErgoTree, SigmaOr, SigmaPropConstant}
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
  val route: Route = ErgoUtilsApiRoute(settings).route
  val p2pkaddress = P2PKAddress(defaultMinerPk)(settings.addressEncoder)
  val p2shaddress = Pay2SHAddress(feeProp)(settings.addressEncoder)
  val p2saddress = Pay2SAddress(feeProp)(settings.addressEncoder)

  val treeSerializer: ErgoTreeSerializer = new ErgoTreeSerializer

  private def jsonStringEntity(value: String) =
    HttpEntity(ContentTypes.`application/json`, value.asJson.noSpaces)

  it should "derive address from ErgoTree (p2s)" in {
    val et = Base16.encode(treeSerializer.serializeErgoTree(p2saddress.script))
    Get(s"$prefix/ergoTreeToAddress/$et") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(p2saddress.toString())
    }
    Post(s"$prefix/ergoTreeToAddress", jsonStringEntity(et)) ~> route ~> check {
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
    Post(s"$prefix/address", jsonStringEntity(p2saddress.toString())) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
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
    Post(s"$prefix/address", jsonStringEntity(invalidAddress)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
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
    Post(s"$prefix/ergoTreeToAddress", jsonStringEntity(et)) ~> route ~> check {
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
    Post(s"$prefix/address", jsonStringEntity(p2shaddress.toString())) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
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
    Post(s"$prefix/address", jsonStringEntity(invalidAddress)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
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

    Post(s"$prefix/addressToRaw", jsonStringEntity(p2pkaddress.toString())) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("raw").as[String] shouldEqual Right(raw)
    }

    Post(s"$prefix/rawToAddress", jsonStringEntity(raw)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(p2pkaddress.toString())
    }
  }

  it should "derive address from ErgoTree (p2pk)" in {
    val et = Base16.encode(treeSerializer.serializeErgoTree(p2pkaddress.script))
    Get(s"$prefix/ergoTreeToAddress/$et") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(p2pkaddress.toString())
    }
    Post(s"$prefix/ergoTreeToAddress", jsonStringEntity(et)) ~> route ~> check {
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
    Post(s"$prefix/address", jsonStringEntity(p2pkaddress.toString())) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
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
    Post(s"$prefix/address", jsonStringEntity(invalidAddress)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
      c.downField("address").as[String] shouldEqual Right(invalidAddress)
      c.downField("isValid").as[Boolean] shouldEqual Right(false)
      c.downField("error").as[String] shouldEqual Right("requirement failed: Trying to decode mainnet address in testnet")
    }
  }

  it should "handle oversized (>2000 chars) ErgoTree and Pay2SAddress via POST endpoints" in {
    val largeProp = SigmaOr(Seq.fill(60)(SigmaPropConstant(defaultMinerPk)))
    val largeTree = ErgoTree.fromProposition(largeProp)
    val largeTreeHex = Base16.encode(treeSerializer.serializeErgoTree(largeTree))
    val largeP2S = Pay2SAddress(largeTree)(settings.addressEncoder)
    val largeP2SStr = largeP2S.toString()

    largeTreeHex.length should be > 2000
    largeP2SStr.length should be > 2000

    Post(s"$prefix/ergoTreeToAddress", jsonStringEntity(largeTreeHex)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(largeP2SStr)
    }

    Post(s"$prefix/address", jsonStringEntity(largeP2SStr)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
      c.downField("address").as[String] shouldEqual Right(largeP2SStr)
      c.downField("isValid").as[Boolean] shouldEqual Right(true)
    }

    Post(s"$prefix/addressToRaw", jsonStringEntity(largeP2SStr)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("raw").as[String] shouldEqual Right(largeTreeHex)
    }
  }

}
