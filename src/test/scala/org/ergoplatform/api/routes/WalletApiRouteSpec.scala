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
import org.ergoplatform.settings.{Args, Constants, ErgoSettings}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, Pay2SAddress, Pay2SHAddress}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class WalletApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/wallet"

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkIdOpt = None))
  val route: Route = WalletApiRoute(readersRef, nodeViewRef, settings).route

  implicit val paymentRequestEncoder: PaymentRequestEncoder = new PaymentRequestEncoder(ergoSettings)
  implicit val assetIssueRequestEncoder: AssetIssueRequestEncoder = new AssetIssueRequestEncoder(ergoSettings)
  implicit val requestsHolderEncoder: RequestsHolderEncoder = new RequestsHolderEncoder(ergoSettings)
  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  implicit val addressJsonDecoder: Decoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).decoder

  val paymentRequest = PaymentRequest(Pay2SAddress(Constants.FalseLeaf), 100L, None, None)
  val assetIssueRequest = AssetIssueRequest(Pay2SAddress(Constants.FalseLeaf), 100L, "TEST", "Test", 8)
  val requestsHolder = RequestsHolder((0 to 10).flatMap(_ => Seq(paymentRequest, assetIssueRequest)), 10000L)
  val scriptSource: String =
    """
      |{
      |    val myPk = PK("3WwUerNahQR1YXyq8AKi5UkKsYeJ99zxrqNqt3BCG4xSGeTERHiQ")
      |    HEIGHT < 9197 && myPk.isProven
      |}
      |""".stripMargin

  val scriptSourceSigProp: String =
    """
      |{
      |    PK("3WwUerNahQR1YXyq8AKi5UkKsYeJ99zxrqNqt3BCG4xSGeTERHiQ")
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
    val suffix = "/p2s_address"
    val assertion = (json: Json) => {
      status shouldBe StatusCodes.OK
      val addressStr = json.hcursor.downField("address").as[String].right.get
      ergoAddressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SAddress.addressTypePrefix
    }
    Post(prefix + suffix, Json.obj("source" -> scriptSource.asJson)) ~> route ~> check(assertion(responseAs[Json]))
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson)) ~> route ~>
      check(assertion(responseAs[Json]))
  }

  it should "generate valid P2SHAddress form source" in {
    val suffix = "/p2sh_address"
    val assertion = (json: Json) => {
      status shouldBe StatusCodes.OK
      val addressStr = json.hcursor.downField("address").as[String].right.get
      ergoAddressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix
    }
    Post(prefix + suffix, Json.obj("source" -> scriptSource.asJson)) ~> route ~> check(assertion(responseAs[Json]))
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson)) ~> route ~>
      check(assertion(responseAs[Json]))
  }

  it should "return addresses" in {
    Get(prefix + "/addresses") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "initialize wallet" in {
    Post(prefix + "/init", Json.obj("pass" -> "1234".asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("mnemonic").as[String] shouldEqual Right(WalletActorStub.mnemonic)
    }
  }

  it should "restore wallet" in {
    Post(prefix + "/restore", Json.obj("pass" -> "1234".asJson, "mnemonic" -> WalletActorStub.mnemonic.asJson)) ~>
      route ~> check(status shouldBe StatusCodes.OK)
  }

  it should "unlock wallet" in {
    Post(prefix + "/unlock", Json.obj("pass" -> "1234".asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "lock wallet" in {
    Get(prefix + "/lock") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "derive new key according to a provided path" in {
    Post(prefix + "/deriveKey", Json.obj("derivationPath" -> "m/1/2".asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(WalletActorStub.address.toString)
    }
  }

  it should "derive next key" in {
    Get(prefix + "/deriveNextKey") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("derivationPath").as[String] shouldEqual Right(WalletActorStub.path.encoded)
      responseAs[Json].hcursor.downField("address").as[String] shouldEqual Right(WalletActorStub.address.toString)
    }
  }

  it should "return all wallet boxes" in {
    val minConfirmations = 15
    val minInclusionHeight = 20
    val postfix = s"/boxes?minConfirmations=$minConfirmations&minInclusionHeight=$minInclusionHeight"
    Get(prefix + postfix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 2
      response.head.hcursor.downField("confirmationsNum").as[Int].forall(_ >= minConfirmations) shouldBe true
      response.head.hcursor.downField("inclusionHeight").as[Int].forall(_ >= minInclusionHeight) shouldBe true
    }
    Get(prefix + "/boxes") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 3
    }
  }


  it should "return unspent wallet boxes" in {
    val minConfirmations = 15
    val minInclusionHeight = 20
    val postfix = s"/boxes/unspent?minConfirmations=$minConfirmations&minInclusionHeight=$minInclusionHeight"
    Get(prefix + postfix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 1
      response.head.hcursor.downField("confirmationsNum").as[Int].forall(_ >= minConfirmations) shouldBe true
      response.head.hcursor.downField("inclusionHeight").as[Int].forall(_ >= minInclusionHeight) shouldBe true
    }
    Get(prefix + "/boxes/unspent") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 2
    }
  }

  it should "return wallet transactions" in {
    Get(prefix + "/transactions") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 2
      responseAs[Seq[ErgoTransaction]] shouldEqual WalletActorStub.walletTxs
    }
  }

}
