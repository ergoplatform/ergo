package org.ergoplatform.http.routes

import java.util.concurrent.atomic.AtomicInteger

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes, UniversalEntity}
import akka.http.scaladsl.server.{Directives, Route, ValidationRejection}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Source
import akka.testkit.TestProbe
import akka.util.ByteString
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.BlocksApiRoute
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.NewBlockMined
import org.ergoplatform.nodeView.LocallyGeneratedModifier
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.api.http.ApiRejectionHandler
import scorex.util.ModifierId

class BlocksApiRouteSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  val prefix = "/blocks"

  val route: Route = BlocksApiRoute(nodeViewRef, digestReadersRef, settings).route
  val handledRoute: Route =
    Directives.handleRejections(ApiRejectionHandler.rejectionHandler) {
      route
    }

  val headerIdBytes: ModifierId = history.lastHeaders(1).headers.head.id
  val headerIdString: String    = Algos.encode(headerIdBytes)

  it should "get last blocks" in {
    Get(prefix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      history
        .headerIdsAt(0, 50)
        .map(Algos.encode)
        .asJson shouldEqual responseAs[Json]
    }
  }

  it should "return method not allowed for PUT /blocks" in {
    Get(prefix) ~> handledRoute ~> check {
      status shouldBe StatusCodes.OK
    }
    Put(prefix) ~> handledRoute ~> check {
      status shouldBe StatusCodes.MethodNotAllowed
      val allow = response.headers.find(_.is("allow")).map(_.value()).getOrElse("")
      allow should include ("GET")
      allow should include ("POST")
    }
  }

  it should "post block correctly" in {
    val (st, bh)             = createUtxoState(settings)
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)
    val blockJson: UniversalEntity =
      HttpEntity(block.asJson.toString).withContentType(ContentTypes.`application/json`)
    Post(prefix, blockJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "announce a posted block before handing it to the node view holder" in {
    val (st, bh)             = createUtxoState(settings)
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)
    val blockJson: UniversalEntity =
      HttpEntity(block.asJson.toString).withContentType(ContentTypes.`application/json`)
    // one probe is both the view holder and the event subscriber, so its queue shows the order the route sent them in
    val probe = TestProbe()
    system.eventStream.subscribe(probe.ref, classOf[NewBlockMined])
    try {
      Post(prefix, blockJson) ~> BlocksApiRoute(probe.ref, digestReadersRef, settings).route ~> check {
        status shouldBe StatusCodes.OK
      }
      probe.expectMsgType[NewBlockMined].header.id shouldBe block.header.id
      probe.expectMsg(LocallyGeneratedModifier(block.header))
    } finally {
      system.eventStream.unsubscribe(probe.ref)
    }
  }

  it should "reject a valid block at unknown sub-paths" in {
    val (st, bh)             = createUtxoState(settings)
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)
    val blockJson: UniversalEntity =
      HttpEntity(block.asJson.toString).withContentType(ContentTypes.`application/json`)

    Post(prefix + "/junk/path", blockJson) ~> route ~> check {
      rejections should contain (ValidationRejection("Wrong modifierId format", None))
    }
    Post(prefix + "/junk/path", blockJson) ~> handledRoute ~> check {
      status shouldBe StatusCodes.BadRequest
    }
    val knownIdSuffix = prefix + "/" + headerIdString + "/junk/path"
    Post(knownIdSuffix, blockJson) ~> handledRoute ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "get last headers" in {
    Get(prefix + "/lastHeaders/1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      history
        .lastHeaders(1)
        .headers
        .map(_.asJson)
        .asJson shouldEqual responseAs[Json]
    }
  }

  it should "get block at height" in {
    Get(prefix + "/at/0") ~> route ~> check {
      status shouldBe StatusCodes.OK
      history
        .headerIdsAtHeight(0)
        .map(Algos.encode)
        .asJson shouldEqual responseAs[Json]
    }
  }

  it should "get chain slice" in {
    Get(prefix + "/chainSlice?fromHeight=0") ~> route ~> check {
      status shouldBe StatusCodes.OK
      chain.map(_.header).asJson shouldEqual responseAs[Json]
    }
    Get(prefix + "/chainSlice?fromHeight=2&toHeight=4") ~> route ~> check {
      status shouldBe StatusCodes.OK
      chain.slice(2, 4).map(_.header).asJson shouldEqual responseAs[Json]
    }
  }

  it should "reject chain slice ranges above the maximum headers limit" in {
    Get(prefix + "/chainSlice?fromHeight=0&toHeight=16385") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "get block by header id" in {
    Get(prefix + "/" + headerIdString) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val expected = history
        .typedModifierById[Header](headerIdBytes)
        .flatMap(history.getFullBlock)
        .map(_.asJson)
        .get

      responseAs[Json] shouldEqual expected
    }
  }

  it should "get blocks by header ids" in {
    val headerIdsBytes               = history.lastHeaders(10).headers
    val headerIdsString: Seq[String] = headerIdsBytes.map(h => Algos.encode(h.id))

    Post(prefix + "/headerIds", headerIdsString.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK

      val expected = headerIdsBytes
        .map(_.id)
        .flatMap(headerId =>
          history.typedModifierById[Header](headerId).flatMap(history.getFullBlock)
        )

      responseAs[Seq[ErgoFullBlock]] shouldEqual expected
    }
  }

  it should "read a streamed header ids body once" in {
    val bytes = ByteString(Seq(headerIdString).asJson.noSpaces)
    val materializations = new AtomicInteger
    val source = Source.single(bytes).mapMaterializedValue { materialized =>
      if (materializations.incrementAndGet() != 1) {
        throw new IllegalStateException("request body materialized twice")
      }
      materialized
    }
    val entity: UniversalEntity =
      HttpEntity.Default(ContentTypes.`application/json`, bytes.length.toLong, source)
    val expected = history
      .typedModifierById[Header](headerIdBytes)
      .flatMap(history.getFullBlock)
      .toSeq
    expected.size shouldBe 1

    Post(prefix + "/headerIds", entity) ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[ErgoFullBlock]] shouldEqual expected
      materializations.get() shouldBe 1
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

      responseAs[Json] shouldEqual expected
    }
  }

  it should "get transactions by header id" in {
    Get(prefix + "/" + headerIdString + "/transactions") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val header    = history.typedModifierById[Header](headerIdBytes).value
      val fullBlock = history.getFullBlock(header).value
      val expected  = fullBlock.blockTransactions.asJson
      responseAs[Json] shouldEqual expected
    }
  }

}
