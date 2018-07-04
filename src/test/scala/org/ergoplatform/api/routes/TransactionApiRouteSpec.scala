package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import org.ergoplatform.api.TransactionsApiRoute
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings
import scorex.crypto.authds.ADKey
import sigmastate.Values.TrueLeaf
import sigmastate.interpreter.{ContextExtension, SerializedProverResult}

import scala.concurrent.duration._

class TransactionApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  import jsonEncoders._

  implicit val timeout = RouteTestTimeout(15.seconds.dilated)

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds)
  val prefix = "/transactions"
  val route = TransactionsApiRoute(readersRef, nodeViewRef, restApiSettings).route

  val input = Input(
    ADKey @@ Array.fill(ErgoBox.BoxId.size)(0: Byte),
    SerializedProverResult(Array.emptyByteArray, ContextExtension(Map())))

  val output = new ErgoBoxCandidate(0, TrueLeaf)
  val tx = ErgoTransaction(IndexedSeq(input), IndexedSeq(output))

  it should "post transaction" in {
    Post(prefix, tx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  //TODO: Not implemented yet
  ignore should "get tx by id" in {
    Get(prefix + "/txod") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "get unconfirmed from mempool" in {
    Get(prefix + "/unconfirmed") ~> route ~> check {
      status shouldBe StatusCodes.OK
      memPool.take(50).toSeq shouldBe responseAs[Seq[ErgoTransaction]]
    }
  }

}
