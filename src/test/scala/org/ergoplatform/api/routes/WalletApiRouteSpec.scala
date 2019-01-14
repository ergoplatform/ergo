package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import io.circe.{Decoder, Json}
import org.ergoplatform.api.WalletApiRoute
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.ErgoAddressJsonEncoder
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, AssetIssueRequestEncoder, PaymentRequest, PaymentRequestEncoder, _}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, Pay2SAddress, Pay2SHAddress}
import org.scalatest.{FlatSpec, Matchers}
import sigmastate.Values

import scala.util.Try

class WalletApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/wallet"

  val ergoSettings: ErgoSettings = ErgoSettings.read(Some("src/test/resources/application.conf"))
  val route: Route = WalletApiRoute(readersRef, nodeViewRef, settings).route

  implicit val paymentRequestEncoder: PaymentRequestEncoder = new PaymentRequestEncoder(ergoSettings)
  implicit val assetIssueRequestEncoder: AssetIssueRequestEncoder = new AssetIssueRequestEncoder(ergoSettings)
  implicit val requestsHolderEncoder: RequestsHolderEncoder = new RequestsHolderEncoder(ergoSettings)
  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonDecoder: Decoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).decoder

  val paymentRequest = PaymentRequest(Pay2SAddress(Values.FalseLeaf), 100L, None, None)
  val assetIssueRequest = AssetIssueRequest(Pay2SAddress(Values.FalseLeaf), 100L, "TEST", "Test", 8)
  val requestsHolder = RequestsHolder((0 to 10).flatMap(_ => Seq(paymentRequest, assetIssueRequest)), 10000L)
  val scriptSource: String =
    """
      |{
      |    val myPk = PK("tJPvNFE9uEJnhD2bzxqPP9X6c9WXfoRNeAvziCvZdBXnqZsHvwxKVG")
      |    HEIGHT < 9197 && myPk.isProven
      |}
      |""".stripMargin

  it should "generate arbitrary transaction" in {
    Post(prefix + "/transaction/generate", requestsHolder.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ErgoTransaction]) shouldBe 'success
    }
  }

  it should "get balances" in {
    Get(prefix + "/balances") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received balances: $json")
      val c = json.hcursor
      c.downField("balance").as[Long] shouldEqual Right(WalletActorStub.confirmedBalance)
    }
  }

  it should "get unconfirmed balances" in {
    Get(prefix + "/balances/with_unconfirmed") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received total confirmed with unconfirmed balances: $json")
      val c = json.hcursor
      c.downField("balance").as[Long] shouldEqual Right(WalletActorStub.unconfirmedBalance)
    }
  }

  it should "generate payment transaction" in {
    Post(prefix + "/payment/generate", Seq(paymentRequest).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ErgoTransaction]) shouldBe 'success
    }
  }

  it should "generate asset issue transaction" in {
    Post(prefix + "/assets/generate", Seq(assetIssueRequest).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ErgoTransaction]) shouldBe 'success
    }
  }

  it should "generate & send arbitrary transaction" in {
    Post(prefix + "/transaction/send", requestsHolder.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "generate & send payment transaction" in {
    Post(prefix + "/payment/send", Seq(paymentRequest).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "generate & send asset issue transaction" in {
    Post(prefix + "/assets/issue", Seq(assetIssueRequest).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "generate valid P2SAddress form source" in {
    Post(prefix + "/p2s_address", Json.obj("source" -> scriptSource.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      ergoAddressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SAddress.addressTypePrefix
    }
  }

  it should "generate valid P2SHAddress form source" in {
    Post(prefix + "/p2sh_address", Json.obj("source" -> scriptSource.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      ergoAddressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix
    }
  }

  it should "return addresses" in {
    Get(prefix + "/addresses") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

}
