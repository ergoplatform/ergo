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
import org.ergoplatform.wallet.crypto.MessageSigning
import scorex.util.encode.Base16
import sigma.serialization.ErgoTreeSerializer
import sigmastate.interpreter.HintsBag

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

  private val signer = defaultProver
  private val signerAddress = P2PKAddress(signer.hdPubKeys.head.key)(settings.addressEncoder)

  private def signedRequest(message: String): Json = {
    val signedMessage = MessageSigning.wrap(message.getBytes("UTF-8"), MessageSigning.freshSalt())
    val proof = signer.signMessage(signer.hdPubKeys.head.key, signedMessage, HintsBag.empty).get
    Json.obj(
      "address" -> Json.fromString(signerAddress.toString),
      "signedMessage" -> Json.fromString(Base16.encode(signedMessage)),
      "proof" -> Json.fromString(Base16.encode(proof))
    )
  }

  it should "verify a signed message" in {
    Post(s"$prefix/verifyMessage", signedRequest("I am the owner of this address")) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
      c.downField("isValid").as[Boolean] shouldEqual Right(true)
      // the caller is told what the signature actually attests to, not just that it is well formed
      c.downField("message").as[String] shouldEqual Right("I am the owner of this address")
    }
  }

  it should "not verify a signed message for another address" in {
    val other = P2PKAddress(signer.hdPubKeys.last.key)(settings.addressEncoder)
    val request = signedRequest("hi").deepMerge(Json.obj("address" -> Json.fromString(other.toString)))
    Post(s"$prefix/verifyMessage", request) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("isValid").as[Boolean] shouldEqual Right(false)
    }
  }

  it should "not verify a message which was not wrapped for signing" in {
    // a proof over bare bytes could be an input proof of a transaction, so it is not accepted here
    val bare = "I am the owner of this address".getBytes("UTF-8")
    val proof = signer.signMessage(signer.hdPubKeys.head.key, bare, HintsBag.empty).get
    val request = Json.obj(
      "address" -> Json.fromString(signerAddress.toString),
      "signedMessage" -> Json.fromString(Base16.encode(bare)),
      "proof" -> Json.fromString(Base16.encode(proof))
    )
    Post(s"$prefix/verifyMessage", request) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val c = responseAs[Json].hcursor
      c.downField("isValid").as[Boolean] shouldEqual Right(false)
      c.downField("message").as[Option[String]] shouldEqual Right(None)
    }
  }

  it should "reject a verification request which does not parse" in {
    Post(s"$prefix/verifyMessage", Json.obj("address" -> Json.fromString(signerAddress.toString))) ~> Route.seal(route) ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

}

