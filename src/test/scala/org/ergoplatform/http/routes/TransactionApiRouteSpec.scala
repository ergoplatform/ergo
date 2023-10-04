package org.ergoplatform.http.routes

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{AdditionalRegisters, NonMandatoryRegisterId, TokenId}
import org.ergoplatform.http.api.{ApiCodecs, TransactionsApiRoute}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetDataFromHistory, GetReaders, Readers}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.Stubs
import org.ergoplatform.{DataInput, ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.settings.RESTApiSettings
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, EvaluatedValue}
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigma.Extensions._
import sigma.ast.SType

import java.net.InetSocketAddress
import scala.concurrent.duration._

class TransactionApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with ApiCodecs
  with FailFastCirceSupport {

  val prefix = "/transactions"

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)
  val route: Route = TransactionsApiRoute(utxoReadersRef, nodeViewRef, settings).route

  val inputBox: ErgoBox = utxoState.takeBoxes(1).head
  val input = Input(inputBox.id, emptyProverResult)
  val dataInput = DataInput(input.boxId)

  val absentModifierId = "0000000000000000000000000000000000000000000000000000000000000000"
  val tokens = List[(TokenId, Long)](inputBox.id.toTokenId -> 10)
  val registers =
    Map(
      ErgoBox.R4 -> ByteArrayConstant("name".getBytes("UTF-8")),
      ErgoBox.R5 -> ByteArrayConstant("4".getBytes("UTF-8")),
    )

  val output: ErgoBoxCandidate =
     new ErgoBoxCandidate(inputBox.value, Constants.TrueLeaf, creationHeight = 0, tokens.toColl, registers)
  val tx: ErgoTransaction = ErgoTransaction(IndexedSeq(input), IndexedSeq(dataInput), IndexedSeq(output))

  val chainedInput = Input(tx.outputs.head.id, emptyProverResult)
  val chainedTx: ErgoTransaction = ErgoTransaction(IndexedSeq(chainedInput), IndexedSeq(output))

  val chainedRoute: Route = {
    //constructing memory pool and node view  with the transaction tx included
    val mp2 = memPool.put(UnconfirmedTransaction(tx, None))
    class UtxoReadersStub2 extends Actor {
      def receive: PartialFunction[Any, Unit] = {
        case GetReaders => sender() ! Readers(history, utxoState, mp2, wallet)
        case GetDataFromHistory(f) => sender() ! f(history)
      }
    }
    val readers2 = system.actorOf(Props(new UtxoReadersStub2))
    TransactionsApiRoute(readers2, nodeViewRef, settings).route
  }

  it should "post transaction" in {
    Post(prefix, tx.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[String] shouldEqual tx.id
    }
  }

  it should "fail when posting invalid transaction" in {
    val failingNodeViewRef = system.actorOf(NodeViewStub.failingProps())
    val failingRoute: Route = TransactionsApiRoute(digestReadersRef, failingNodeViewRef, settings).route

    Post(prefix, tx.asJson) ~> failingRoute ~> check {
      status shouldBe StatusCodes.BadRequest
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

  it should "get unconfirmed txs from mempool" in {
    Get(prefix + "/unconfirmed") ~> route ~> check {
      status shouldBe StatusCodes.OK
      memPool.take(50).map(_.transaction).toSeq shouldBe responseAs[Seq[ErgoTransaction]]
    }
  }

  it should "get unconfirmed from mempool using limit and offset" in {
    val limit = 10
    val offset = 20
    Get(prefix + s"/unconfirmed?limit=$limit&offset=$offset") ~> route ~> check {
      status shouldBe StatusCodes.OK
      memPool.getAll.slice(offset, offset + limit).map(_.transaction) shouldBe responseAs[Seq[ErgoTransaction]]
    }
  }

  it should "return unconfirmed tx by output ergoTree from mempool" in {
    val searchedBox = txs.head.outputs.head.ergoTree.bytesHex
    Post(prefix + s"/unconfirmed/byErgoTree", searchedBox) ~> route ~> check {
      val expectedTxs = memPool.getAll.toSet.filter(_.transaction.outputs.exists(_.ergoTree.bytesHex == searchedBox))
      val actualTxs = responseAs[Set[ErgoTransaction]]
      status shouldBe StatusCodes.OK
      expectedTxs.map(_.id) shouldBe actualTxs.map(_.id)
      actualTxs.forall(tx => tx.outputs.map(_.ergoTree.bytesHex).contains(searchedBox)) shouldBe true
    }
  }

  it should "return unconfirmed tx by input ergoTree from mempool" in {
    val searchedBox = inputBox.ergoTree.bytesHex
    Post(prefix + s"/unconfirmed/byErgoTree", searchedBox) ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      val txs = responseAs[Set[ErgoTransaction]]
      txs.forall { tx =>
        tx.inputs.map(_.boxId).exists(_.sameElements(inputBox.id)) ||
          tx.outputs.map(_.ergoTree.bytesHex).contains(searchedBox)
      } shouldBe true
    }
  }

  it should "return unconfirmed tx by absent ergoTree from mempool" in {
    Post(prefix + s"/unconfirmed/byErgoTree", absentModifierId) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[ErgoTransaction]] shouldBe List.empty
    }
  }

  it should "return unconfirmed tx by id from mempool" in {
    Get(prefix + s"/unconfirmed/byTransactionId/${txs.head.id}") ~> route ~> check {
      status shouldBe StatusCodes.OK
      memPool.modifierById(txs.head.id).get shouldBe responseAs[ErgoTransaction]
    }
  }

  it should "return unconfirmed tx by absent id from mempool" in {
    Get(prefix + s"/unconfirmed/byTransactionId/$absentModifierId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "return 200 if unconfirmed tx is present" in {
    Head(prefix + s"/unconfirmed/${txs.head.id}") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "return 404 if unconfirmed absent tx is present" in {
    Head(prefix + s"/unconfirmed/$absentModifierId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "return unconfirmed input by boxId from mempool" in {
    val searchedBox = inputBox.id
    val searchedBoxEncoded = Base16.encode(searchedBox)
    Get(prefix + s"/unconfirmed/inputs/byBoxId/$searchedBoxEncoded") ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(searchedBoxEncoded)
    }
  }

  it should "return unconfirmed output by boxId from mempool" in {
    val searchedBox = txs.head.outputs.head.id
    val searchedBoxEncoded = Base16.encode(searchedBox)
    Get(prefix + s"/unconfirmed/outputs/byBoxId/$searchedBoxEncoded") ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(searchedBoxEncoded)
    }
  }

  it should "return unconfirmed output by ergoTree from mempool" in {
    val searchedBox = txs.head.outputs.head
    val searchedTree = searchedBox.ergoTree.bytesHex
    Post(prefix + s"/unconfirmed/outputs/byErgoTree", searchedTree) ~> route ~> check {
      val expectedOutputIds =
        memPool.getAll
          .flatMap(_.transaction.outputs)
          .filter(_.ergoTree.bytesHex == searchedTree)
          .map(b => Base16.encode(b.id)).take(50).toList
      status shouldBe StatusCodes.OK
      val actualOutputIds = responseAs[List[Json]].map(_.hcursor.downField("boxId").as[String].right.get)
      actualOutputIds shouldEqual expectedOutputIds
    }
  }

  it should "return unconfirmed output by tokenId from mempool" in {
    val searchedToken = tokens.head._1.toHex
    Get(prefix + s"/unconfirmed/outputs/byTokenId/$searchedToken") ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      val actualOutputIds = responseAs[List[Json]].map(_.hcursor.downField("boxId").as[String].right.get)
      actualOutputIds.nonEmpty shouldBe true
      actualOutputIds shouldEqual List(Base16.encode(tx.outputs.head.id))
    }
  }

  it should "return unconfirmed outputs by exact same registers" in {
    val searchedRegs =
      Map(
        ErgoBox.R4 -> ByteArrayConstant("name".getBytes("UTF-8")),
        ErgoBox.R5 -> ByteArrayConstant("4".getBytes("UTF-8")),
      ).asInstanceOf[AdditionalRegisters].asJson

    Post(prefix + s"/unconfirmed/outputs/byRegisters", searchedRegs) ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      val actualBoxes = responseAs[List[Json]].flatMap(_.hcursor.downField("boxId").as[String].toOption)
      actualBoxes.head shouldEqual Base16.encode(tx.outputs.head.id)
    }
  }

  it should "return unconfirmed outputs by subset of registers" in {
    val searchedRegs =
      Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]](
        ErgoBox.R4 -> ByteArrayConstant("name".getBytes("UTF-8"))
      ).asJson

    Post(prefix + s"/unconfirmed/outputs/byRegisters", searchedRegs) ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      val actualBoxes = responseAs[List[Json]].flatMap(_.hcursor.downField("boxId").as[String].toOption)
      actualBoxes.head shouldEqual Base16.encode(tx.outputs.head.id)
    }
  }

  it should "not return unconfirmed output by registers when one missing" in {
    val searchedRegs =
      Map(
        ErgoBox.R4 -> ByteArrayConstant("name".getBytes("UTF-8")),
        ErgoBox.R5 -> ByteArrayConstant("4".getBytes("UTF-8")),
        ErgoBox.R6 -> ByteArrayConstant("description".getBytes("UTF-8")),
      ).asInstanceOf[AdditionalRegisters].asJson

    Post(prefix + s"/unconfirmed/outputs/byRegisters", searchedRegs) ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      val actualBoxes = responseAs[List[Json]].flatMap(_.hcursor.downField("boxId").as[String].toOption)
      actualBoxes shouldEqual List.empty
    }
  }

  it should "not return unconfirmed outputs by registers with only key match" in {
    val searchedRegs =
      Map[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]](
        ErgoBox.R4 -> ByteArrayConstant("foo".getBytes("UTF-8"))
      ).asJson

    Post(prefix + s"/unconfirmed/outputs/byRegisters", searchedRegs) ~> chainedRoute ~> check {
      status shouldBe StatusCodes.OK
      val actualBoxes = responseAs[List[Json]].flatMap(_.hcursor.downField("boxId").as[String].toOption)
      actualBoxes shouldEqual List.empty
    }
  }

  it should "not return unconfirmed outputs by empty registers" in {
    val searchedRegs = Map.empty[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]].asJson
    Post(prefix + s"/unconfirmed/outputs/byRegisters", searchedRegs) ~> chainedRoute ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

}
