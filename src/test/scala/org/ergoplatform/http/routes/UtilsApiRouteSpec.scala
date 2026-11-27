package org.ergoplatform.http.routes

import java.net.InetSocketAddress
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
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
  val route: Route = ErgoUtilsApiRoute(settings).route
  val p2pkaddress = P2PKAddress(defaultMinerPk)(settings.addressEncoder)
  val p2shaddress = Pay2SHAddress(feeProp)(settings.addressEncoder)
  val p2saddress = Pay2SAddress(feeProp)(settings.addressEncoder)

  val treeSerializer: ErgoTreeSerializer = new ErgoTreeSerializer

  it should "generate a 32 byte seed by default" in {
    Get(s"$prefix/seed") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val seed = responseAs[Json].as[String].toOption.get
      Base16.decode(seed).get.length shouldBe 32
    }
  }

  it should "generate seeds up to the maximum requested length" in {
    Seq(0, 32, 1024).foreach { length =>
      Get(s"$prefix/seed/$length") ~> route ~> check {
        status shouldBe StatusCodes.OK
        val seed = responseAs[Json].as[String].toOption.get
        Base16.decode(seed).get.length shouldBe length
      }
    }
  }

  it should "reject invalid seed lengths" in {
    Seq("-1", "1025", Int.MaxValue.toString, "not-a-number").foreach { length =>
      Get(s"$prefix/seed/$length") ~> route ~> check {
        status shouldBe StatusCodes.BadRequest
        val detail = responseAs[Json].hcursor.downField("detail").as[String]
        detail shouldBe Right("Seed length must be an integer between 0 and 1024 bytes")
      }
    }
  }

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

}

