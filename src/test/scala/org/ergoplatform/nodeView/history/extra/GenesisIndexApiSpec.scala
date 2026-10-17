package org.ergoplatform.nodeView.history.extra

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import org.ergoplatform.http.api.BlockchainApiRoute
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.bytesToId

class GenesisIndexApiSpec extends AnyFlatSpec with Matchers with ScalatestRouteTest
  with FailFastCirceSupport with Stubs {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  it should "serve positive genesis references and bounded ranges without exposing reserved zero" in {
    GenesisIndex.initialize(history.historyStorage, settings.chainSettings, rentEnabled = false).get
    val route = akka.http.scaladsl.server.Route.seal(BlockchainApiRoute(digestReadersRef,
      settings.copy(nodeSettings = settings.nodeSettings.copy(extraIndex = true)), None).route)
    val expected = org.ergoplatform.nodeView.state.ErgoState.genesisBoxes(settings.chainSettings).map(box => bytesToId(box.id))
    Seq(0L, -1L, 4L, Long.MaxValue).foreach { number =>
      Get(s"/blockchain/box/byIndex/$number") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
    expected.zipWithIndex.foreach { case (id, index) =>
      Get(s"/blockchain/box/byIndex/${index + 1}") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[io.circe.Json].hcursor.get[String]("boxId").toOption.get shouldBe id
      }
    }
    Seq((0, 5, expected.reverse), (2, 5, expected.take(1)), (3, 5, Seq.empty),
      (Int.MaxValue, 5, Seq.empty), (0, 0, Seq.empty)).foreach { case (offset, limit, ids) =>
      Get(s"/blockchain/box/range?offset=$offset&limit=$limit") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[Seq[String]] shouldBe ids
      }
    }
    Seq("offset=-1", "limit=-1").foreach { invalid =>
      Get(s"/blockchain/box/range?$invalid") ~> route ~> check {
        status shouldBe StatusCodes.BadRequest
      }
    }
  }

  it should "retain zero as transient metadata for unconfirmed boxes" in {
    val tree = memPool.getAll.head.transaction.outputs.head.ergoTree
    val address = IndexedErgoAddress(IndexedErgoAddressSerializer.hashErgoTree(tree))
    val expected = memPool.getAll.flatMap(_.transaction.outputs).filter(_.ergoTree == tree).map(box => bytesToId(box.id))
    expected should not be empty
    val returned = address.retrieveUtxos(history, memPool, 0, 10000,
      org.ergoplatform.http.api.SortDirection.ASC, unconfirmed = true, Set.empty)
    returned.map(_.id).toSet shouldBe expected.toSet
    returned.forall(_.globalIndex == 0L) shouldBe true
    NumericBoxIndex.getBoxByNumber(history, 0L) shouldBe None
  }
}
