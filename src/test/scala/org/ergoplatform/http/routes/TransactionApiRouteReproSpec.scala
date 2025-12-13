package org.ergoplatform.http.routes

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.http.api.{ApiCodecs, TransactionsApiRoute}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.settings.RESTApiSettings
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import org.ergoplatform.settings.Constants.TrueTree
import sigma.Extensions.ArrayOps
import java.net.InetSocketAddress
import scala.concurrent.duration._

class TransactionApiRouteReproSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with ApiCodecs
  with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  val prefix = "/transactions"

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)
  
  val inputBox: ErgoBox = utxoState.takeBoxes(1).head
  val input = Input(inputBox.id, emptyProverResult)
  
  val emptyTokens = Array[(TokenId, Long)]()

  // Output C
  val outputC: ErgoBoxCandidate =
     new ErgoBoxCandidate(inputBox.value, TrueTree, creationHeight = 0, emptyTokens.toColl, Map.empty)
     
  // Tx1: Input A -> Output C
  val tx1: ErgoTransaction = ErgoTransaction(IndexedSeq(input), IndexedSeq(), IndexedSeq(outputC))

  // Tx2: Input C -> Output E
  val inputC = Input(tx1.outputs.head.id, emptyProverResult)
  val outputE: ErgoBoxCandidate = 
      new ErgoBoxCandidate(inputBox.value, TrueTree, creationHeight = 0, emptyTokens.toColl, Map.empty)
  val tx2: ErgoTransaction = ErgoTransaction(IndexedSeq(inputC), IndexedSeq(), IndexedSeq(outputE))

  // Route setup with both transactions in mempool
  val chainedRoute: Route = {
    // constructing memory pool with both transactions
    val mp2 = memPool.put(UnconfirmedTransaction(tx1, None)).put(UnconfirmedTransaction(tx2, None))
    
    class UtxoReadersStub2 extends Actor {
      def receive: PartialFunction[Any, Unit] = {
        case GetReaders => sender() ! Readers(history, utxoState, mp2, wallet)
        case GetDataFromHistory(f) => sender() ! f(history)
      }
    }
    val readers2 = system.actorOf(Props(new UtxoReadersStub2))
    TransactionsApiRoute(readers2, nodeViewRef, settings).route
  }

  it should "return unconfirmed input by boxId for chained transaction (repro)" in {
    val searchedBoxId = tx1.outputs.head.id
    val searchedBoxEncoded = Base16.encode(searchedBoxId)
    
    Get(prefix + s"/unconfirmed/inputs/byBoxId/$searchedBoxEncoded") ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(searchedBoxEncoded)
    }
  }
}
