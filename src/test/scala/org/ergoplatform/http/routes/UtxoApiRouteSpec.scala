package org.ergoplatform.http.routes

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.{ApiCodecs, UtxoApiRoute}
import org.ergoplatform.mining.InputBlockFields
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{BoxHolder, StateType, UtxoState}
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.{HistoryTestHelpers, RandomWrapper, Stubs}
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import org.ergoplatform.utils.generators.ValidBlocksGenerators.validTransactionsFromBoxes
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import scorex.util.bytesToId
import sigma.Colls
import sigma.ast.ErgoTree
import sigma.data.TrivialProp.TrueProp

class UtxoApiRouteSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with ApiCodecs {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  val prefix = "/utxo"

  val route: Route =
    UtxoApiRoute(utxoReadersRef, utxoSettings.scorexSettings.restApi).route

  /**
    * Creates a fresh local history and state with a transaction applied as an input block.
    * Returns a route backed by that history and the input-block transaction.
    */
  private def inputBlockRoute: (Route, ErgoTransaction) = {
    val testBox1 = new org.ergoplatform.ErgoBox(
      value = 1000000000L,
      ergoTree = ErgoTree.fromProposition(TrueProp),
      creationHeight = 0,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Blake2b256.hash("testBoxA")),
      index = 0
    )
    val testBox2 = new org.ergoplatform.ErgoBox(
      value = 1000000000L,
      ergoTree = ErgoTree.fromProposition(TrueProp),
      creationHeight = 0,
      additionalTokens = Colls.emptyColl,
      additionalRegisters = Map.empty,
      transactionId = bytesToId(Blake2b256.hash("testBoxB")),
      index = 1
    )

    val bh = BoxHolder(Seq(testBox1, testBox2))
    val us = UtxoState.fromBoxHolder(bh, None, createTempDir, utxoSettings, parameters)

    val h = HistoryTestHelpers.generateHistory(
      verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false,
      blocksToKeep = -1, epochLength = 10000, useLastEpochs = 3,
      initialDiffOpt = None, None)
    val chain = genChain(2, h, stateOpt = Some(us))
    applyChain(h, chain)

    val tx = validTransactionsFromBoxes(10000, Seq(bh.boxes.head._2), new RandomWrapper(Some(1)))._1.head

    val c2 = genChain(2, h, stateOpt = Some(us)).tail
    val inputBlock = InputBlockAnnouncement(1, c2(0).header, InputBlockFields.empty, None)
    h.applyInputBlock(inputBlock) shouldBe None
    val (newBest, _) = h.applyInputBlockTransactions(inputBlock.id, Seq(tx), us)
    newBest should contain(inputBlock.id)

    val readers = Readers(h, us, ErgoMemPool.empty(utxoSettings), wallet)
    val readersRef = system.actorOf(Props(new Actor {
      def receive: Receive = { case GetReaders => sender() ! readers }
    }))
    (UtxoApiRoute(readersRef, utxoSettings.scorexSettings.restApi).route, tx)
  }

  it should "get utxo box with /byId" in {
    val box   = utxoState.takeBoxes(1).head
    val boxId = Base16.encode(box.id)
    Get(prefix + s"/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("value").as[Long] shouldEqual Right(box.value)
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(boxId)
    }
  }

  it should "get mempool box with withPool/byId" in {
    val box   = memPool.getAll.map(utx => utx.transaction).flatMap(_.outputs).head
    val boxId = Base16.encode(box.id)
    Get(prefix + s"/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
    Get(prefix + s"/withPool/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("value").as[Long] shouldEqual Right(box.value)
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(boxId)
    }
  }

  it should "get all mempool boxes with withPool/byIds" in {
    val boxes = memPool.getAll.map(utx => utx.transaction).flatMap(_.outputs)
    val boxesEncoded = boxes.map(box => Base16.encode(box.id))

    Post(prefix + "/withPool/byIds", boxesEncoded.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[Json]]
        .map(_.hcursor.downField("value").as[Long]) shouldEqual boxes.map(x => Right(x.value))
      responseAs[Seq[Json]]
        .map(_.hcursor.downField("boxId").as[String]) shouldEqual boxesEncoded.map(x => Right(x))
    }
  }

  it should "get input-block box with withPool/byId" in {
    val (routeWithInputBlock, tx) = inputBlockRoute
    val boxId = Base16.encode(tx.outputs.head.id)
    Get(prefix + s"/byId/$boxId") ~> routeWithInputBlock ~> check {
      status shouldBe StatusCodes.NotFound
    }
    Get(prefix + s"/withPool/byId/$boxId") ~> routeWithInputBlock ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(boxId)
      responseAs[Json].hcursor.downField("value").as[Long].isRight shouldBe true
    }
  }

  it should "get input-block boxes with withPool/byIds" in {
    val (routeWithInputBlock, tx) = inputBlockRoute
    val boxId = Base16.encode(tx.outputs.head.id)
    Post(prefix + "/withPool/byIds", Seq(boxId).asJson) ~> routeWithInputBlock ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[Seq[Json]]
      response.size shouldBe 1
      response.head.hcursor.downField("boxId").as[String].toOption.get shouldBe boxId
    }
  }

  it should "get input-block box with withPool/byIdBinary" in {
    val (routeWithInputBlock, tx) = inputBlockRoute
    val boxId = Base16.encode(tx.outputs.head.id)
    Get(prefix + s"/withPool/byIdBinary/$boxId") ~> routeWithInputBlock ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String].toOption.get shouldBe boxId
    }
  }

  it should "not return input-block spent input with withPool/byId" in {
    val (routeWithInputBlock, tx) = inputBlockRoute
    val spentBoxId = Base16.encode(tx.inputs.head.boxId)
    Get(prefix + s"/withPool/byId/$spentBoxId") ~> routeWithInputBlock ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "not found utxo box with /byId" in {
    val boxId = Base16.encode(Blake2b256(utxoState.takeBoxes(1).head.id))
    Get(prefix + s"/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "get utxo box with /byIdBinary" in {
    val box   = utxoState.takeBoxes(1).head
    val boxId = Base16.encode(box.id)
    Get(prefix + s"/byIdBinary/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(boxId)
      val bytes = Base16
        .decode(responseAs[Json].hcursor.downField("bytes").as[String].toOption.get)
        .get
      val boxRestored = ErgoBoxSerializer.parseBytes(bytes)
      box shouldEqual boxRestored
    }
  }

  it should "not found utxo box with /byIdBinary" in {
    val boxId = Base16.encode(Blake2b256(utxoState.takeBoxes(1).head.id))
    Get(prefix + s"/byId/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "get pool box with /withPool/byIdBinary" in {
    val box   = memPool.getAll.map(utx => utx.transaction).flatMap(_.outputs).head
    val boxId = Base16.encode(box.id)
    Get(prefix + s"/byIdBinary/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
    Get(prefix + s"/withPool/byIdBinary/$boxId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("boxId").as[String] shouldEqual Right(boxId)
      val bytes = Base16
        .decode(responseAs[Json].hcursor.downField("bytes").as[String].toOption.get)
        .get
      val boxRestored = ErgoBoxSerializer.parseBytes(bytes)
      box shouldEqual boxRestored
    }
  }

  it should "/genesis returns 3 boxes" in {
    Get(prefix + s"/genesis") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[List[Json]]
      response.size shouldBe 3 // 3 genesis boxes as per Ergo Whitepaper
    }
  }

  it should "get serialized proof for given boxes" in {
    val boxes = utxoState.takeBoxes(10).map(box => Base16.encode(box.id))
    Post(prefix + s"/getBoxesBinaryProof", boxes.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }
}
