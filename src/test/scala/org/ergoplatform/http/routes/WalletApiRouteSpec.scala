package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import io.circe.{Decoder, Json}
import org.ergoplatform.http.api.{ApiCodecs, WalletApiRoute}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequestEncoder, PaymentRequest, PaymentRequestEncoder, _}
import org.ergoplatform.nodeView.wallet.{AugWalletTransaction, ErgoAddressJsonEncoder}
import org.ergoplatform.settings.{Args, Constants, ErgoSettings}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.utils.generators.ErgoTransactionGenerators
import org.ergoplatform.{ErgoAddress, Pay2SAddress}
import org.ergoplatform.wallet.{Constants => WalletConstants}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Random, Try}
import scala.concurrent.duration._
import akka.http.scaladsl.server.MissingQueryParamRejection

class WalletApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with ApiCodecs {

  implicit val timeout: RouteTestTimeout = RouteTestTimeout(145.seconds)

  val prefix = "/wallet"

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))
  val route: Route = WalletApiRoute(digestReadersRef, nodeViewRef, settings).route
  val failingNodeViewRef = system.actorOf(NodeViewStub.failingProps())
  val failingRoute: Route = WalletApiRoute(digestReadersRef, failingNodeViewRef, settings).route

  val utxoRoute: Route = WalletApiRoute(utxoReadersRef, nodeViewRef, settings).route

  implicit val paymentRequestEncoder: PaymentRequestEncoder = new PaymentRequestEncoder(ergoSettings)
  implicit val assetIssueRequestEncoder: AssetIssueRequestEncoder = new AssetIssueRequestEncoder(ergoSettings)
  implicit val requestsHolderEncoder: RequestsHolderEncoder = new RequestsHolderEncoder(ergoSettings)
  implicit val addressJsonDecoder: Decoder[ErgoAddress] = ErgoAddressJsonEncoder(settings).decoder

  val paymentRequest = PaymentRequest(Pay2SAddress(Constants.FalseLeaf), 100L, Seq.empty, Map.empty)
  val assetIssueRequest = AssetIssueRequest(Pay2SAddress(Constants.FalseLeaf), None, 100L, "TEST", "Test", 8)
  val requestsHolder = RequestsHolder(
    (0 to 10).flatMap(_ => Seq(paymentRequest, assetIssueRequest)),
    Some(10000L),
    Seq.empty,
    Seq.empty,
    minerRewardDelay = 720
  )


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
    Get(prefix + "/balances/withUnconfirmed") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received total confirmed with unconfirmed balances: $json")
      val c = json.hcursor
      c.downField("balance").as[Long] shouldEqual Right(WalletActorStub.unconfirmedBalance)
    }
  }

  it should "generate & send arbitrary transaction" in {
    Post(prefix + "/transaction/send", requestsHolder.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "fail when sent transaction is invalid" in {
    Post(prefix + "/transaction/send", requestsHolder.asJson) ~> failingRoute ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "sign a transaction" in {
    val digest = Random.nextBoolean()
    val (tsr, r) = if (digest) {
      (ErgoTransactionGenerators.transactionSigningRequestGen(true).sample.get, route)
    } else {
      (ErgoTransactionGenerators.transactionSigningRequestGen(utxoState).sample.get, utxoRoute)
    }
    Post(prefix + "/transaction/sign", tsr.asJson) ~> r ~> check {
      status shouldBe StatusCodes.OK
      responseAs[ErgoTransaction].id shouldBe tsr.unsignedTx.id
    }
  }

  it should "generate & send payment transaction" in {
    Post(prefix + "/payment/send", Seq(paymentRequest).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "fail when payment is invalid" in {
    Post(prefix + "/payment/send", Seq(paymentRequest).asJson) ~> failingRoute ~> check {
      status shouldBe StatusCodes.BadRequest
    }
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
    Post(prefix + "/restore", Json.obj("pass" -> "1234".asJson, "mnemonic" -> WalletActorStub.mnemonic.asJson, 
      "usePre1627KeyDerivation" -> false.asJson)) ~>
      route ~> check(status shouldBe StatusCodes.OK)
  }

  it should "not restore wallet without key derivation method specified" in {
    Post(prefix + "/restore", Json.obj("pass" -> "1234".asJson, "mnemonic" -> WalletActorStub.mnemonic.asJson)) ~>
      route ~> check(rejection shouldBe a[MissingQueryParamRejection])
  }

  it should "unlock wallet" in {
    Post(prefix + "/unlock", Json.obj("pass" -> "1234".asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "check wallet" in {
    Post(prefix + "/check", Json.obj("mnemonic" -> WalletActorStub.mnemonic.asJson)) ~>
      route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("matched").as[Boolean] shouldBe Right(true)
    }
  }

  it should "lock wallet" in {
    Get(prefix + "/lock") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "rescan wallet post" in {
    Post(prefix + "/rescan") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "rescan wallet post with fromHeight" in {
    Post(prefix + "/rescan", Json.obj("fromHeight" -> 0.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }

    Post(prefix + "/rescan", Json.obj("fromHeight" -> (-1).asJson)) ~> route ~> check {
      rejection shouldEqual ValidationRejection("fromHeight field must be >= 0", None)
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

  it should "return wallet boxes" in {
    Get(prefix + "/boxes") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 3
    }
  }

  it should "return wallet boxes with lower constraint" in {
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
  }

  it should "return wallet boxes with upper constraint" in {
    val maxConfirmations = 15
    val maxInclusionHeight = 20
    val postfix = s"/boxes?maxConfirmations=$maxConfirmations&maxInclusionHeight=$maxInclusionHeight"
    Get(prefix + postfix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 1
      response.head.hcursor.downField("confirmationsNum").as[Int].forall(_ <= maxConfirmations) shouldBe true
      response.head.hcursor.downField("inclusionHeight").as[Int].forall(_ <= maxInclusionHeight) shouldBe true
    }
  }

  it should "return wallet boxes with both lower and upper constraint" in {
    val confirmations = 15
    val inclusionHeight = 20
    val postfix = s"/boxes?$confirmations&minInclusionHeight=$inclusionHeight&maxConfirmations=$confirmations&maxInclusionHeight=$inclusionHeight"
    Get(prefix + postfix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.isEmpty shouldBe true
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
      val walletTxs = WalletActorStub.walletTxs.filter { awtx =>
        awtx.wtx.scanIds.exists(_ <= WalletConstants.PaymentsScanId)
      }

      response.size shouldBe walletTxs.size
      responseAs[Seq[AugWalletTransaction]] shouldEqual walletTxs
    }
  }

  it should "return wallet transactions by scanId" in {
    Get(prefix + "/transactionsByScanId/1") ~> route ~> check {
      import AugWalletTransaction._
      status shouldBe StatusCodes.OK
      val response = responseAs[List[AugWalletTransaction]]
      val walletTxs = response.filter { awtx =>
        awtx.wtx.scanIds.contains(1.shortValue())
      }
      walletTxs.size shouldBe response.size
    }
  }

  it should "return wallet transactions by scanId including unconfirmed txs" in {
    Get(prefix + "/transactionsByScanId/1?includeUnconfirmed=true") ~> route ~> check {
      import AugWalletTransaction._
      status shouldBe StatusCodes.OK
      val response = responseAs[List[AugWalletTransaction]]
      val walletTxs = response.filter { awtx =>
        awtx.wtx.scanIds.contains(1.shortValue())
      }
      walletTxs.size shouldBe response.size
      walletTxs.forall(_.numConfirmations == 0) shouldBe true
    }
  }

  it should "get lock status" in {
    Get(prefix + "/status") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[Json]
      response.hcursor.downField("isUnlocked").as[Boolean] shouldBe Right(true)
      response.hcursor.downField("isInitialized").as[Boolean] shouldBe Right(true)
    }
  }

}
