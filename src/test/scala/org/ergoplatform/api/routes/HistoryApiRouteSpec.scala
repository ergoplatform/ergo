package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.parser._
import io.circe.syntax._
import org.ergoplatform.api.services.HistoryService
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{BlockTransactions, Header, HeaderChain}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.ErgoSettings
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.ModifierId
import scorex.core.api.http.{ApiError, SuccessApiResponse}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.concurrent.Future

class HistoryApiRouteSpec extends FlatSpec with Matchers with ScalatestRouteTest {

  import HistoryServiceMock._

  val service = HistoryServiceMock

  val routes = HistoryApiRoute(service, ErgoSettings.read(None).scorexSettings).route

  val base = "/history/"

  Get(base + "height") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(Map("height" -> 200).asJson).toJson shouldEqual json
  }

  Post(base + "height") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(Map("height" -> 200).asJson).toJson shouldEqual json
  }

  Get(base + "best-header") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(bestHeader.json).toJson shouldEqual json
  }

  Post(base + "best-header") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(bestHeader.json).toJson shouldEqual json
  }

  Get(base + "best-full-block") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(bestFullBlock.json).toJson shouldEqual json
  }

  Post(base + "best-full-block") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(bestFullBlock.json).toJson shouldEqual json
  }

  Get(base + "last-headers/10") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(Map("headers" -> headerChain.headers.map(_.json)).asJson).toJson shouldEqual json
  }

  Post(base + "last-headers/10") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(Map("headers" -> headerChain.headers.map(_.json)).asJson).toJson shouldEqual json
  }

  Get(base + "modifier/id") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    ApiError(404, "not-found").toJson shouldEqual json
  }

  Post(base + "modifier/id") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    ApiError(404, "not-found").toJson shouldEqual json
  }

  Get(base + "current-difficulty") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(Map("difficulty" -> 100).asJson).toJson shouldEqual json
  }

  Post(base + "current-difficulty") ~> routes ~> check {
    status shouldEqual StatusCodes.OK
    val resp = responseAs[String]
    val json = parse(resp).right.get
    SuccessApiResponse(Map("difficulty" -> 100).asJson).toJson shouldEqual json
  }
}

object HistoryServiceMock extends HistoryService {

  override def getHeight = Future.successful(200)

  override def getBestHeader = Future.successful(Some(bestHeader))

  override def getBestFullBlock = Future.successful(Some(bestFullBlock))

  override def getLastHeaders(n: Int) = Future.successful(headerChain)

  override def getModifierById(id: String) = Future.successful(None)

  override def getCurrentDifficulty = Future.successful(BigInt(100))

  val bestHeader = Header(
    15.toByte,
    ModifierId @@ Array.fill(32)(0.toByte),
    Seq.empty,
    Digest32 @@ Array.fill(32)(1.toByte),
    ADDigest @@ Array.fill(33)(2.toByte),
    Digest32 @@ Array.fill(32)(3.toByte),
    System.currentTimeMillis(),
    1L,
    100,
    Array.empty[Byte],
    101L,
    Array.empty[Byte])

  val tx = AnyoneCanSpendTransaction(IndexedSeq(201L), IndexedSeq(202L))

  val bestFullBlock = ErgoFullBlock(
    bestHeader,
    BlockTransactions(bestHeader.id, Seq(tx)),
    None
  )

  val headerChain = HeaderChain(Seq(bestHeader))
}
