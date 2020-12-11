package org.ergoplatform.http.routes

import java.net.InetSocketAddress

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import org.ergoplatform.http.api.TransactionsApiRoute
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{DataInput, ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

class TransactionApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/transactions"

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds)
  val route: Route = TransactionsApiRoute(utxoReadersRef, nodeViewRef, restApiSettings).route

  val inputBox: ErgoBox = utxoState.takeBoxes(1).head
  val input = Input(inputBox.id, emptyProverResult)
  val dataInput = DataInput(input.boxId)

  val output: ErgoBoxCandidate = new ErgoBoxCandidate(inputBox.value, Constants.TrueLeaf, creationHeight = 0)
  val tx: ErgoTransaction = ErgoTransaction(IndexedSeq(input), IndexedSeq(dataInput), IndexedSeq(output))

  val chainedInput = Input(tx.outputs.head.id, emptyProverResult)
  val chainedTx: ErgoTransaction = ErgoTransaction(IndexedSeq(chainedInput), IndexedSeq(output))

  val chainedRoute: Route = {
    //constructing memory pool and node view  with the transaction tx included
    val mp2 = memPool.put(tx).get
    class UtxoReadersStub2 extends Actor {
      def receive: PartialFunction[Any, Unit] = {
        case GetReaders => sender() ! Readers(history, utxoState, mp2, wallet)
        case GetDataFromHistory(f) => sender() ! f(history)
      }
    }
    val readers2 = system.actorOf(Props(new UtxoReadersStub2))
    TransactionsApiRoute(readers2, nodeViewRef, restApiSettings).route
  }

  it should "post transaction" in {
    Post(prefix, tx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual tx.id
    }
  }

  it should "post chained transactions" in {
    Post(prefix, chainedTx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }

    Post(prefix, tx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual tx.id
    }

    Post(prefix, chainedTx.asJson) ~> chainedRoute ~> check {
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

      Post(prefix + "/check", chainedTx.asJson) ~> route ~> check {
        status shouldBe StatusCodes.BadRequest
      }

      Post(prefix + "/check", chainedTx.asJson) ~> chainedRoute ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String] shouldEqual chainedTx.id
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
