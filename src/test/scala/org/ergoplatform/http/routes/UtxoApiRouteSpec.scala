package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.{ApiCodecs, UtxoApiRoute}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16

class UtxoApiRouteSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with ApiCodecs {

  val prefix = "/utxo"

  val route: Route =
    UtxoApiRoute(utxoReadersRef, utxoSettings.scorexSettings.restApi).route

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
