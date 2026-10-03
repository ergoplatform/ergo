package org.ergoplatform.nodeView.history.extra

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.RollbackToKey
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.ByteBuffer

class BlockchainExtraIndexGateSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val indexedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = true))
  private val route = BlockchainApiRoute(digestReadersRef, indexedSettings, None).route
  private val txPath = "/blockchain/transaction/byId/" + ("00" * 32)
  private val boxPath = "/blockchain/box/byId/" + ("00" * 32)

  it should "withhold extra-index rows while rollback recovery is incomplete" in {
    Get(txPath) ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
    Get(boxPath) ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
    history.historyStorage.insertExtraTry(
      Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(1).array), Array.empty).get
    try {
      Get("/blockchain/indexedHeight") ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
      Get(txPath) ~> route ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
      Get(boxPath) ~> route ~> check {
        status shouldBe StatusCodes.InternalServerError
      }
    } finally {
      history.historyStorage.insertExtraTry(
        Array(RollbackToKey -> ByteBuffer.allocate(4).putInt(0).array), Array.empty).get
    }
    Get(txPath) ~> route ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }
}
