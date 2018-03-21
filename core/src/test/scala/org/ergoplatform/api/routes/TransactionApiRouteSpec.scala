package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import io.circe.syntax._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

class TransactionApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs  {

  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10 seconds)
  val prefix = "/transactions"
  val route = TransactionsApiRoute(readersRef, nodeViewRef, restApiSettings).route

  val tx = AnyoneCanSpendTransaction(IndexedSeq(1L,2L), IndexedSeq(3L, 4L))

  //TODO: Not fully implemented yet. There is no codec for tx.
  ignore should "post transaction" in {
    Post(prefix, tx.json.toString()) ~> route ~> check {
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
      memPool.take(50).toSeq.map(_.json).asJson.toString shouldBe responseAs[String]
    }
  }

}
