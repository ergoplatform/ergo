package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import io.circe.syntax._
import org.ergoplatform.api.BlocksApiRoute
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.settings.Algos
import org.scalatest.OptionValues._
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.ModifierId

import scala.concurrent.duration._

class BlocksApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  val prefix = "/blocks"
  val route = BlocksApiRoute(readersRef, minerRef, settings, nodeId).route


  it should "get last blocks" in {
    Get(prefix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      history.headerIdsAt(50, 0).map(Algos.encode).asJson.toString() shouldEqual responseAs[String]
    }
  }

  ignore should "post block correclty" in {
    //TODO wait until method will be implemented on api
    Post(prefix) ~> route ~> check {
      ???
    }
  }

  it should "get last headers" in {
    Get(prefix + "/lastHeaders/1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      history.lastHeaders(1, 0).headers.map(_.asJson).asJson.toString() shouldEqual responseAs[String]
    }
  }

  it should "get block at height" in {
    Get(prefix + "/at/0") ~> route ~> check {
      status shouldBe StatusCodes.OK
      history.headerIdsAtHeight(0).map(Algos.encode).asJson.toString() shouldEqual responseAs[String]
    }
  }

  it should "get candidate block info" in {
    Get(prefix + "/candidateBlock") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val res = Map(
        "isMining" -> false.asJson,
        "candidateBlock" -> None.asJson
      ).asJson
      res.toString shouldEqual responseAs[String]
    }
  }

  val headerIdBytes: ModifierId = history.lastHeaders(1,0).headers.head.id
  val headerIdString: String = Algos.encode(headerIdBytes)

  ignore should "get block by header id" in {
    Get(prefix + "/" + headerIdString) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val expected = history.typedModifierById[Header](headerIdBytes)
        .flatMap(history.getFullBlock)
        .map(_.asJson)
        .get
        .toString
      responseAs[String] shouldEqual expected
    }
  }

  it should "get header by header id" in {
    Get(prefix + "/" + headerIdString + "/header") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val expected = history
        .typedModifierById[Header](headerIdBytes)
        .flatMap(history.getFullBlock)
        .map(_.header.asJson)
        .get
        .toString
      responseAs[String] shouldEqual expected
    }
  }

  ignore should "get transactions by header id" in {
    Get(prefix + "/" + headerIdString + "/transactions") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val header = history.typedModifierById[Header](headerIdBytes).value
      val fullBlock = history.getFullBlock(header).value
      val blockTransactions = fullBlock.blockTransactions
      val expected = fullBlock.blockTransactions.asJson.toString
      responseAs[String] shouldEqual expected
    }
  }
}
