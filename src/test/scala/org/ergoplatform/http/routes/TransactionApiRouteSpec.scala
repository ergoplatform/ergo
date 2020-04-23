package org.ergoplatform.http.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import org.ergoplatform.http.api.TransactionsApiRoute
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{DataInput, ErgoBoxCandidate, Input}
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

class TransactionApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/transactions"

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds)
  val route: Route = TransactionsApiRoute(utxoReadersRef, nodeViewRef, restApiSettings).route

  val inputBox = utxoState.takeBoxes(1).head
  val input = Input(inputBox.id, emptyProverResult)
  val dataInput = DataInput(input.boxId)

  val output: ErgoBoxCandidate = new ErgoBoxCandidate(inputBox.value, Constants.TrueLeaf, creationHeight = 0)
  val tx: ErgoTransaction = ErgoTransaction(IndexedSeq(input), IndexedSeq(dataInput), IndexedSeq(output))

  val chainedInput = Input(tx.outputs.head.id, emptyProverResult)
  val chainedTx: ErgoTransaction = ErgoTransaction(IndexedSeq(chainedInput), IndexedSeq(output))

  it should "post transaction" in {
    Post(prefix, tx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual tx.id
    }
  }

  it should "post chained transactions" in {
    Post(prefix, tx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual tx.id
    }

    Post(prefix, chainedTx.asJson) ~> route ~> check {
      println(responseAs[String])
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual chainedTx.id
    }
  }

  it should "check transaction" in {
      Post(prefix + "/check", tx.asJson) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String] shouldEqual tx.id
      }
      //second attempt should be fine also
      Post(prefix + "/check", tx.asJson) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String] shouldEqual tx.id
      }
  }

  it should "get unconfirmed from mempool" in {
    Get(prefix + "/unconfirmed") ~> route ~> check {
      status shouldBe StatusCodes.OK
      memPool.take(50).toSeq shouldBe responseAs[Seq[ErgoTransaction]]
    }
  }

  it should "get unconfirmed from mempool using limit and offset" in {
    val limit = 10
    val offset = 20
    Get(prefix + s"/unconfirmed?limit=$limit&offset=$offset") ~> route ~> check {
      status shouldBe StatusCodes.OK
      memPool.getAll.slice(offset, offset + limit) shouldBe responseAs[Seq[ErgoTransaction]]
    }
  }

}
