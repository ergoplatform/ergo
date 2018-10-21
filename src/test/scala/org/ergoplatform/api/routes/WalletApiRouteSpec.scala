package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import org.ergoplatform.api.WalletApiRoute
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.wallet._
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, AssetIssueRequestEncoder, PaymentRequest, PaymentRequestEncoder}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.Stubs
import org.scalatest.{FlatSpec, Matchers, TryValues}
import sigmastate.Values

import scala.util.Try

class WalletApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with TryValues {

  val prefix = "/wallet"

  val ergoSettings: ErgoSettings = ErgoSettings.read(Some("src/test/resources/application.conf"))
  val route: Route = WalletApiRoute(readersRef, nodeViewRef, settings).route

  implicit val paymentRequestEncoder: PaymentRequestEncoder = new PaymentRequestEncoder(ergoSettings)
  implicit val assetIssueRequestEncoder: AssetIssueRequestEncoder = new AssetIssueRequestEncoder(ergoSettings)
  implicit val ergoAddressEncoder: ErgoAddressEncoder = new ErgoAddressEncoder(ergoSettings)

  it should "generate payment transaction" in {
    val request = PaymentRequest(Pay2SAddress(Values.FalseLeaf), 100L, None, None, 100000L)
    Post(prefix + "/payment/generate", Seq(request).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ErgoTransaction]) shouldBe 'success
    }
  }

  it should "generate asset issue transaction" in {
    val request = AssetIssueRequest(Pay2SAddress(Values.FalseLeaf), 100L, "TEST", "Test", 8, 100000L)
    Post(prefix + "/assets/generate", Seq(request).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ErgoTransaction]) shouldBe 'success
    }
  }

  it should "generate & send payment transaction" in {
    val request = PaymentRequest(Pay2SAddress(Values.FalseLeaf), 100L, None, None, 100000L)
    Post(prefix + "/payment/send", Seq(request).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] should not be empty
    }
  }

  it should "generate & send asset issue transaction" in {
    val request = AssetIssueRequest(Pay2SAddress(Values.FalseLeaf), 100L, "TEST", "Test", 8, 100000L)
    Post(prefix + "/assets/issue", Seq(request).asJson) ~> route ~> check {
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
