package org.ergoplatform.http.routes

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.http.api.NipopowApiRoute
import org.ergoplatform.modifiers.history.popow.NipopowProof
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.utils.Stubs
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NipopowApiRoutesSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

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

  it should "reject proof request when minimum and suffix length overflow" in {
    Get(s"/nipopow/proof/${Int.MaxValue}/1") ~> route ~> check {
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
    val proof = history.popowProof(1, 1, Some(existingHeaderId)).get
    proof.hasValidParams shouldBe true
    proof.suffixHead.id shouldBe existingHeaderId
    proof.suffixTail shouldBe empty
    val proofBytes = history.popowProofBytes(1, 1, Some(existingHeaderId)).get
    val parsed = history.nipopowSerializer.parseBytes(proofBytes)
    parsed.hasValidParams shouldBe true
    parsed.suffixHeaders.map(_.id) shouldBe proof.suffixHeaders.map(_.id)
    history.nipopowSerializer.toBytes(parsed).toSeq shouldBe proofBytes.toSeq

    Get(s"/nipopow/proof/1/1/$existingHeaderId") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received nipopow proof response : $json")
      val c = json.hcursor
      c.downField("prefix").as[List[Json]].right.get shouldNot be(empty)
    }
  }

  it should "reject a proof for the current tip when k requires a successor" in {
    val tip = history.bestHeaderOpt.get
    val proof = history.popowProof(1, 2, Some(tip.id))
    proof.isFailure shouldBe true
    proof.failed.get.getMessage should include("suffix length 1")

    val proofBytes = history.popowProofBytes(1, 2, Some(tip.id))
    proofBytes.isFailure shouldBe true
    proofBytes.failed.get.getMessage should include("suffix length 1")

    Get(s"/nipopow/proof/1/2/${tip.id}") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[Json].hcursor.downField("detail").as[String].right.get should
        include("suffix length 1")
    }
  }

  it should "return a proof when exactly the requested suffix is available" in {
    val tip = history.bestHeaderOpt.get
    val suffixHead = history.bestHeaderAtHeight(tip.height - 1).get
    val proof = history.popowProof(1, 2, Some(suffixHead.id)).get
    proof.hasValidParams shouldBe true
    proof.suffixHead.id shouldBe suffixHead.id
    proof.suffixTail.map(_.id) shouldBe Seq(tip.id)
    val proofBytes = history.popowProofBytes(1, 2, Some(suffixHead.id)).get
    val parsed = history.nipopowSerializer.parseBytes(proofBytes)
    parsed.hasValidParams shouldBe true
    parsed.suffixHeaders.map(_.id) shouldBe proof.suffixHeaders.map(_.id)
    history.nipopowSerializer.toBytes(parsed).toSeq shouldBe proofBytes.toSeq

    Get(s"/nipopow/proof/1/2/${suffixHead.id}") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val cursor = responseAs[Json].hcursor
      cursor.downField("k").as[Int].right.get shouldBe 2
      cursor.downField("suffixHead").downField("header").downField("id")
        .as[String].right.get shouldBe suffixHead.id
      val tail = cursor.downField("suffixTail").as[List[Json]].right.get
      tail.map(_.hcursor.downField("id").as[String].right.get) shouldBe List(tip.id)
    }
  }

  it should "reject a disconnected selected suffix while retaining a one-header fork proof" in {
    val tip = history.bestHeaderOpt.get
    val selectedHead = history.bestHeaderAtHeight(tip.height - 1).get
    val forkParent = chain.find(_.header.height == tip.height - 2).get
    val sibling = genChain(1, forkParent).tail.head
    sibling.header.height shouldBe selectedHead.height
    sibling.header.id should not be selectedHead.id
    applyChain(history, Seq(sibling))

    history.popowHeader(sibling.id).get.header.id shouldBe sibling.id
    history.bestHeaderAtHeight(sibling.header.height).get.id shouldBe selectedHead.id
    history.bestHeaderOpt.get.id shouldBe tip.id
    history.bestHeadersAfter(sibling.header, 1).map(_.id) shouldBe Seq(tip.id)
    tip.parentId should not be sibling.id

    val disconnected = history.popowProof(1, 2, Some(sibling.id))
    disconnected.isFailure shouldBe true
    disconnected.failed.get.getMessage should include("disconnected suffix")
    val disconnectedBytes = history.popowProofBytes(1, 2, Some(sibling.id))
    disconnectedBytes.isFailure shouldBe true
    disconnectedBytes.failed.get.getMessage should include("disconnected suffix")
    Get(s"/nipopow/proof/1/2/${sibling.id}") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[Json].hcursor.downField("detail").as[String].right.get should
        include("disconnected suffix")
    }

    val oneHeaderProof = history.popowProof(1, 1, Some(sibling.id)).get
    oneHeaderProof.hasValidParams shouldBe true
    oneHeaderProof.hasValidConnections shouldBe true
    oneHeaderProof.suffixHead.id shouldBe sibling.id
    oneHeaderProof.suffixTail shouldBe empty
    val oneHeaderBytes = history.popowProofBytes(1, 1, Some(sibling.id)).get
    val parsed = history.nipopowSerializer.parseBytes(oneHeaderBytes)
    parsed.suffixHeaders.map(_.id) shouldBe Seq(sibling.id)
    parsed.hasValidConnections shouldBe true
    Get(s"/nipopow/proof/1/1/${sibling.id}") ~> route ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Json].hcursor.downField("suffixHead").downField("header")
        .downField("id").as[String].right.get shouldBe sibling.id
    }
  }

  it should "keep retarget headers on the requested retained fork" in {
    val forkSettings = settings.copy(
      directory = createTempDir.getAbsolutePath,
      chainSettings = settings.chainSettings.copy(
        epochLength = 20, useLastEpochs = 2, eip37EpochLength = None),
      nodeSettings = settings.nodeSettings.copy(extraIndex = false, blocksToKeep = -1)
    )
    val forkHistory = ErgoHistory.readOrGenerate(forkSettings)(null)
    val readers = system.actorOf(Props(new Actor {
      def receive: Receive = {
        case GetDataFromHistory(f) => sender() ! f(forkHistory)
      }
    }))
    try {
      val selected = genChain(22)
      applyChain(forkHistory, selected)
      val fork = genChain(20, selected.head).tail
      applyChain(forkHistory, fork)
      val requested = fork.last.header
      val forkRetarget = fork.find(_.header.height == 20).get.header
      val selectedRetarget = selected.find(_.header.height == 20).get.header

      requested.height shouldBe 21
      forkHistory.bestHeaderOpt.get.id shouldBe selected.last.id
      forkHistory.popowHeader(requested.id).get.id shouldBe requested.id
      forkHistory.bestHeaderAtHeight(20).get.id shouldBe selectedRetarget.id
      forkRetarget.id should not be selectedRetarget.id
      val selectedProof = forkHistory.popowProof(1, 1, Some(selected.last.id)).get
      selectedProof.hasValidConnections shouldBe true
      selectedProof.prefix.find(_.height == 20).get.id shouldBe selectedRetarget.id

      val proof = forkHistory.popowProof(1, 1, Some(requested.id)).get
      proof.continuous shouldBe true
      val bytes = forkHistory.popowProofBytes(1, 1, Some(requested.id)).get
      val parsed = forkHistory.nipopowSerializer.parseBytes(bytes)

      val forkRoute = NipopowApiRoute(nodeViewRef, readers, forkSettings).route
      var httpProof: Option[NipopowProof] = None
      Get(s"/nipopow/proof/1/1/${requested.id}") ~> forkRoute ~> check {
        status shouldBe StatusCodes.OK
        httpProof = Some(responseAs[Json].as(NipopowProof.nipopowProofDecoder(proof.popowAlgos)).right.get)
      }
      val returnedProofs = Seq(proof, parsed, httpProof.get)
      withClue(s"Producer, binary roundtrip and HTTP connection checks: ${returnedProofs.map(_.hasValidConnections)}. ") {
        returnedProofs.foreach { returned =>
          returned.prefix.find(_.height == 20).get.id shouldBe forkRetarget.id
          returned.hasValidConnections shouldBe true
          returned.hasValidDifficultyHeaders shouldBe true
        }
      }
    } finally {
      system.stop(readers)
      forkHistory.closeStorage()
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

