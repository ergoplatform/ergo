package org.ergoplatform.http.routes

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes, UniversalEntity}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.AuthorizationFailedRejection
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.testkit.TestProbe
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.BlocksApiRoute
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.ResetBlockchainTo
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import scorex.util.ModifierId
import scala.concurrent.duration._
import scala.util.Failure

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

  it should "reject reset without a valid configured API key before contacting the node" in {
    val probe = TestProbe()
    val secured = settings.copy(scorexSettings = settings.scorexSettings.copy(
      restApi = settings.scorexSettings.restApi.copy(
        apiKeyHash = Some("e1c7ef7b3b742c5ae8f52c24d2c5f5c5dccd9c23a41fa6e76e5a6b62c8f72a10")
      )
    ))
    val protectedRoute = BlocksApiRoute(probe.ref, digestReadersRef, secured).route
    val request = Post(prefix + "/reset", Json.obj("height" -> 1.asJson))
    request ~> protectedRoute ~> check {
      rejections should contain(AuthorizationFailedRejection)
    }
    request.withHeaders(RawHeader("api_key", "incorrect-test-key")) ~> protectedRoute ~> check {
      rejections should contain(AuthorizationFailedRejection)
    }
    probe.expectNoMessage(100.millis)
  }

  it should "report node reset failures as errors rather than successful resets" in {
    val probe = TestProbe()
    val failingRoute = BlocksApiRoute(probe.ref, digestReadersRef, settings).route
    val result = Post(prefix + "/reset", Json.obj("height" -> 1.asJson)) ~> failingRoute ~> runRoute
    probe.expectMsg(ResetBlockchainTo(1))
    probe.reply(Failure(new IllegalStateException("rollback unavailable")))
    check {
      status shouldBe StatusCodes.InternalServerError
      responseAs[Json].hcursor.get[String]("detail").toOption.get should include("rollback unavailable")
    }(result)
  }

  it should "forward authenticated reset requests and preserve node validation errors" in {
    val probe = TestProbe()
    val key = "reset-route-test-key"
    val secured = settings.copy(scorexSettings = settings.scorexSettings.copy(
      restApi = settings.scorexSettings.restApi.copy(apiKeyHash = Some(Algos.encode(Blake2b256(key))))
    ))
    val protectedRoute = BlocksApiRoute(probe.ref, digestReadersRef, secured).route
    val request = Post(prefix + "/reset", Json.obj("height" -> 1.asJson))
      .withHeaders(RawHeader("api_key", key))
    val result = request ~> protectedRoute ~> runRoute
    probe.expectMsg(ResetBlockchainTo(1))
    probe.reply(Failure(new IllegalArgumentException("target version unavailable")))
    check {
      status shouldBe StatusCodes.BadRequest
      responseAs[Json].hcursor.get[String]("detail").toOption.get should include("target version unavailable")
    }(result)
  }

  it should "not add a method rejection for unrelated reset subpaths" in {
    val resetRoute = BlocksApiRoute(nodeViewRef, digestReadersRef, settings).resetBlockchainR
    Get("/junk/path") ~> Route.seal(resetRoute) ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  it should "keep reset restricted to POST after matching the reset path" in {
    val resetRoute = BlocksApiRoute(nodeViewRef, digestReadersRef, settings).resetBlockchainR
    Get("/reset") ~> Route.seal(resetRoute) ~> check {
      status shouldBe StatusCodes.MethodNotAllowed
    }
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

  it should "post block correctly" in {
    val (st, bh)             = createUtxoState(settings)
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)
    val blockJson: UniversalEntity =
      HttpEntity(block.asJson.toString).withContentType(ContentTypes.`application/json`)
    Post(prefix, blockJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
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


  it should "reject reset with negative height" in {
    val before = (history.headersHeight, history.bestHeaderIdOpt, history.bestFullBlockIdOpt)
    val resetRequestJson = Map("height" -> -1).asJson
    val resetRequestEntity = HttpEntity(resetRequestJson.toString).withContentType(ContentTypes.`application/json`)

    Post(prefix + "/reset", resetRequestEntity) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      val response = responseAs[Json]
      val detail = response.hcursor.get[String]("detail").getOrElse("")
      detail should include("Height must be non-negative")
      (history.headersHeight, history.bestHeaderIdOpt, history.bestFullBlockIdOpt) shouldBe before
    }
  }

  it should "reject reset with height higher than current" in {
    val before = (history.headersHeight, history.bestHeaderIdOpt, history.bestFullBlockIdOpt)
    val currentHeight = history.headersHeight
    val targetHeight = currentHeight + 100

    val resetRequestJson = Map("height" -> targetHeight).asJson
    val resetRequestEntity = HttpEntity(resetRequestJson.toString).withContentType(ContentTypes.`application/json`)

    Post(prefix + "/reset", resetRequestEntity) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      val response = responseAs[Json]
      val detail = response.hcursor.get[String]("detail").getOrElse("")
      detail should include("higher than current height")
      (history.headersHeight, history.bestHeaderIdOpt, history.bestFullBlockIdOpt) shouldBe before
    }
  }

  it should "invalidate block transactions, reset to deeper block, and re-apply blocks as valid" in {
    import org.ergoplatform.consensus.{ModifierSemanticValidity, ProgressInfo}
    import org.ergoplatform.nodeView.state.StateType
    import org.ergoplatform.utils.generators.ChainGenerator.applyBlock

    val localHistory = generateHistory(
      verifyTransactions = true,
      stateType = StateType.Utxo,
      poPoWBootstrap = false,
      blocksToKeep = -1
    )
    val fullChain = chain.take(5)
    fullChain.foreach(b => applyBlock(localHistory, b))
    localHistory.headersHeight shouldBe 5
    localHistory.fullBlockHeight shouldBe 5

    val corruptedBlock = fullChain(3) // height 4
    val tipBlock = fullChain(4)       // height 5

    // 1. Invalidate block transactions at height 4 via reportModifierIsInvalid
    localHistory.reportModifierIsInvalid(
      corruptedBlock.blockTransactions,
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    ).isSuccess shouldBe true

    localHistory.isSemanticallyValid(corruptedBlock.id) shouldBe ModifierSemanticValidity.Invalid
    localHistory.isSemanticallyValid(corruptedBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Invalid
    localHistory.isSemanticallyValid(tipBlock.id) shouldBe ModifierSemanticValidity.Invalid

    // 2. Reset to deeper block (height 2)
    val targetHeight = 2
    localHistory.resetToHeight(targetHeight).isSuccess shouldBe true
    localHistory.headersHeight shouldBe targetHeight
    localHistory.fullBlockHeight shouldBe targetHeight
    localHistory.bestHeaderIdOpt shouldBe Some(fullChain(1).id)
    localHistory.bestFullBlockIdOpt shouldBe Some(fullChain(1).id)

    fullChain.drop(targetHeight).foreach { removedBlock =>
      localHistory.contains(removedBlock.id) shouldBe false
      localHistory.contains(removedBlock.blockTransactions.id) shouldBe false
      localHistory.isSemanticallyValid(removedBlock.id) shouldBe ModifierSemanticValidity.Absent
      localHistory.isSemanticallyValid(removedBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Absent
    }

    // 3. Re-apply blocks 3, 4, 5 and mark valid — they must be accepted and become Valid again
    fullChain.drop(targetHeight).foreach { b =>
      applyBlock(localHistory, b)
      localHistory.reportModifierIsValid(b).isSuccess shouldBe true
    }

    localHistory.headersHeight shouldBe 5
    localHistory.fullBlockHeight shouldBe 5
    localHistory.bestFullBlockIdOpt shouldBe Some(tipBlock.id)
    localHistory.isSemanticallyValid(corruptedBlock.id) shouldBe ModifierSemanticValidity.Valid
    localHistory.isSemanticallyValid(corruptedBlock.blockTransactions.id) shouldBe ModifierSemanticValidity.Valid
    localHistory.isSemanticallyValid(tipBlock.id) shouldBe ModifierSemanticValidity.Valid
  }

  it should "reset blockchain to specified height via POST /blocks/reset" in {
    val currentHeight = history.headersHeight
    currentHeight should be > 1
    val targetHeight = currentHeight - 1
    val removedIds = history.headerIdsAtHeight(currentHeight)
    removedIds should not be empty

    val resetRequestJson = Map("height" -> targetHeight).asJson
    val resetRequestEntity = HttpEntity(resetRequestJson.toString).withContentType(ContentTypes.`application/json`)

    Post(prefix + "/reset", resetRequestEntity) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[Json]
      val responseObj = response.asObject.get
      responseObj("success").get.asBoolean.get shouldBe true
      responseObj("resetHeight").get.asNumber.get.toInt.get shouldBe targetHeight
      responseObj("message").get.asString.get should include("validity indices cleaned")
      history.headersHeight shouldBe targetHeight
      removedIds.foreach(id => history.contains(id) shouldBe false)
    }
  }
}
