package org.ergoplatform.http.routes.llm_generated

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.BlocksApiRoute
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId

import java.lang.reflect.{InvocationHandler, Method, Proxy}

class MatrixInputBlockApiRouteSpec
  extends AnyFlatSpec
    with Matchers
    with ScalatestRouteTest
    with FailFastCirceSupport
    with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants.settings

  private val inputId = bytesToId(Array.fill[Byte](32)(17))
  private val inputTransactions = txs.take(2)

  // Supply normal input-block API results while retaining the existing history
  // fixture for full-block lookups; this test isolates HTTP route dispatch.
  private val inputHistory = Proxy.newProxyInstance(
    classOf[ErgoHistoryReader].getClassLoader,
    Array[Class[_]](classOf[ErgoHistoryReader]),
    new InvocationHandler {
      override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = {
        val args = Option(arguments).getOrElse(Array.empty[AnyRef])
        method.getName match {
          case "getInputBlockTransactions" if args.headOption.contains(inputId) =>
            Some(inputTransactions)
          case "getInputBlockTransactionIds" if args.headOption.contains(inputId) =>
            Some(inputTransactions.map(_.id))
          case _ => method.invoke(history, args: _*)
        }
      }
    }
  ).asInstanceOf[ErgoHistoryReader]

  private val readers = system.actorOf(Props(new Actor {
    override def receive: Receive = {
      case GetDataFromHistory(f) => sender() ! f(inputHistory)
    }
  }))

  private val route: Route = BlocksApiRoute(nodeViewRef, readers, settings).route

  "Matrix block routes" should "return transactions for an input block id" in {
    inputTransactions should not be empty
    Get(s"/blocks/$inputId/inputBlockTransactions") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json] shouldBe inputTransactions.asJson
    }
  }

  it should "return transaction ids for an input block id" in {
    inputTransactions should not be empty
    Get(s"/blocks/$inputId/inputBlockTransactionIds") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json] shouldBe inputTransactions.map(tx => Algos.encode(tx.id)).asJson
    }
  }

  it should "keep full-block lookup without a trailing slash" in {
    val block = chain.last
    Get(s"/blocks/${block.id}") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json] shouldBe block.asJson
    }
  }

  it should "keep full-block lookup with a trailing slash" in {
    val block = chain.last
    Get(s"/blocks/${block.id}/") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json] shouldBe block.asJson
    }
  }
}
