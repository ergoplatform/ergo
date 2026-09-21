package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigma.serialization.ErgoTreeSerializer

class BlockchainApiRouteSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val prefix = "/blockchain"
  private val routeSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = true))
  private val route: Route = BlockchainApiRoute(digestReadersRef, routeSettings, indexerOpt = None).route
  private val sealedRoute: Route = Route.seal(route)
  private val treeSerializer = ErgoTreeSerializer.DefaultSerializer

  it should "accept a canonical ErgoTree filter body" in {
    val treeHex = Base16.encode(treeSerializer.serializeErgoTree(feeProp))

    Post(prefix + "/box/byErgoTree", treeHex) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "reject malformed ErgoTree filter bodies" in {
    Post(prefix + "/box/byErgoTree", "00") ~> sealedRoute ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "reject ErgoTree filter bodies with trailing bytes" in {
    val treeHex = Base16.encode(treeSerializer.serializeErgoTree(feeProp))

    Post(prefix + "/box/byErgoTree", treeHex + "00") ~> route ~> check {
      rejection shouldEqual ValidationRejection("ErgoTree bytes contain trailing data", None)
    }
  }
}
