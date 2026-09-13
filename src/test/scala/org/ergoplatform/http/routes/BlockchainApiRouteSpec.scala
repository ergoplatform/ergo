package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockchainApiRouteSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  val prefix = "/blockchain"

  val route: Route = {
    val indexedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = true))
    BlockchainApiRoute(digestReadersRef, indexedSettings, None).route
  }

  val modifierId: String = Algos.encode(Array.fill(32)(0: Byte))

  it should "reject negative paging limits" in {
    Get(prefix + "/box/range?limit=-1") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }

    Get(prefix + "/transaction/range?limit=-1") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "reject negative paging offsets" in {
    Get(prefix + s"/box/byTokenId/$modifierId?offset=-1") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "cap token-indexed box queries" in {
    Get(prefix + s"/box/byTokenId/$modifierId?limit=16385") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "reject invalid template unspent sort direction" in {
    Get(prefix + s"/box/unspent/byTemplateHash/$modifierId?sortDirection=sideways") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "return not found for missing transaction and box indexes" in {
    Get(prefix + "/transaction/byIndex/999999999") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }

    Get(prefix + "/box/byIndex/999999999") ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "return empty ranges when the paging offset is past indexed data" in {
    Get(prefix + "/transaction/range?offset=999999999&limit=1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[String]] shouldBe Seq.empty
    }

    Get(prefix + "/box/range?offset=999999999&limit=1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[String]] shouldBe Seq.empty
    }
  }

  it should "cap batch token info queries" in {
    Post(prefix + "/tokens", Seq.fill(16385)(modifierId).asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }
}
