package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.http.api.NipopowApiRoute
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NipopowApiRoutesSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  private val route: Route = NipopowApiRoute(nodeViewRef, utxoReadersRef, settings).route

  it should "return proof for min superchain & suffix length" in {
    Get("/nipopow/proof/1/1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received nipopow proof response : $json")
      val c = json.hcursor
      c.downField("prefix").as[List[Json]].right.get shouldNot be(empty)
    }
  }

  it should "proof request with invalid minimum and suffix length" in {
    Get("/nipopow/proof/12/24") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "proof request with missing headerId" in {
    Get("/nipopow/proof/1/1/05bf63aa1ecfc9f4e3fadc993f87b33edb4d58e151c1891816d734dd5a0e2e09") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "proof request with invalid headerId" in {
    Get("/nipopow/proof/1/1/x") ~> route ~> check {
      rejection shouldEqual ValidationRejection("Wrong modifierId format", None)
    }
  }

  it should "return proof for min superchain & suffix length & header id" in {
    val existingHeaderId = history.bestHeaderOpt.get.id
    Get(s"/nipopow/proof/1/1/$existingHeaderId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received nipopow proof response : $json")
      val c = json.hcursor
      c.downField("prefix").as[List[Json]].right.get shouldNot be(empty)
    }
  }

  it should "get popow header by id" in {
    val existingHeaderId = history.bestHeaderOpt.get.id
    Get(s"/nipopow/popowHeaderById/$existingHeaderId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received popow header response : $json")
      val c = json.hcursor
      c.downField("interlinks").as[List[Json]].right.get shouldNot be(empty)
    }
  }

  it should "get popow header by height" in {
    Get(s"/nipopow/popowHeaderByHeight/2") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received popow header response : $json")
      val c = json.hcursor
      c.downField("interlinks").as[List[Json]].right.get shouldNot be(empty)
    }
  }

}

