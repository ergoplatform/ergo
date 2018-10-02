package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.api.WalletApiRoute
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.{FlatSpec, Matchers}
import sigmastate.Values

class WalletApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/wallet"

  val ergoSettings = ErgoSettings.read(None)
  implicit val requestEncoder = new PaymentRequestEncoder(ergoSettings)
  val route = WalletApiRoute(readersRef, nodeViewRef, settings).route

  private implicit val ergoAddressEncoder = new ErgoAddressEncoder(ergoSettings)

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

  it should "generate transaction" in {
    val amount = 100L
    val request = PaymentRequest(Pay2SAddress(Values.FalseLeaf), amount, None, None)
    Post(prefix + "/transaction/generate", Seq(request).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[ErgoTransaction].outputs.head.value shouldEqual amount
    }
  }

  it should "generate & send transaction" in {
    val request = PaymentRequest(Pay2SAddress(Values.FalseLeaf), 100L, None, None)
    Post(prefix + "/transaction/payment", Seq(request).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "return addresses" in {
    Get(prefix + "/addresses") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

}
