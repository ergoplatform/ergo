package org.ergoplatform.http.routes

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.consensus.{ModifierSemanticValidity, ProgressInfo}
import org.ergoplatform.http.api.BlocksApiRoute
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.GetDataFromHistory
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.HistoryTestHelpers.generateHistory
import org.ergoplatform.utils.ScorexEncoding
import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StoredHeaderApiRouteSpec extends AnyFlatSpec
  with Matchers with ScalatestRouteTest with FailFastCirceSupport with ScorexEncoding {

  private lazy val chain = genChain(2)
  private lazy val block = chain.last
  private lazy val headerPath = s"/blocks/${block.header.id}/header"

  private def withHistory(includeBody: Boolean)(test: (ErgoHistory, Route) => Unit): Unit = {
    val history = generateHistory(verifyTransactions = true, StateType.Digest, PoPoWBootstrap = false, blocksToKeep = 100)
    val readers = system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case GetDataFromHistory(f) => sender() ! f(history)
      }
    }))
    try {
      if (includeBody) applyChain(history, chain)
      else chain.foreach(b => history.append(b.header).get)
      val route = Route.seal(BlocksApiRoute(ActorRef.noSender, readers, settings).route)
      test(history, route)
    } finally {
      system.stop(readers)
      history.closeStorage()
    }
  }

  it should "serve a stored header when its block body is absent" in {
    withHistory(includeBody = false) { (history, route) =>
      history.typedModifierById[Header](block.header.id).map(_.id) shouldBe Some(block.header.id)
      history.getFullBlock(block.header) shouldBe None

      Get(headerPath) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[Json] shouldBe block.header.asJson
      }
      Get(s"/blocks/${block.header.id}") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }

  it should "preserve the header response when the full block is stored" in {
    withHistory(includeBody = true) { (history, route) =>
      history.getFullBlock(block.header).isDefined shouldBe true
      Get(headerPath) ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[Json] shouldBe block.header.asJson
      }
    }
  }

  it should "return not found for an unknown header" in {
    withHistory(includeBody = false) { (_, route) =>
      Get(s"/blocks/${"00" * 32}/header") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }

  it should "return not found for a stored modifier of another type" in {
    withHistory(includeBody = true) { (history, route) =>
      history.modifierById(block.blockTransactions.id).isDefined shouldBe true
      Get(s"/blocks/${block.blockTransactions.id}/header") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }

  it should "keep a stored semantically invalid header hidden" in {
    withHistory(includeBody = true) { (history, route) =>
      val progress = ProgressInfo[BlockSection](Some(block.header.parentId), Seq(block), Seq.empty, Seq.empty)
      history.reportModifierIsInvalid(block.header, progress).get
      history.contains(block.header.id) shouldBe true
      history.isSemanticallyValid(block.header.id) shouldBe ModifierSemanticValidity.Invalid

      Get(headerPath) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }
  }

  it should "preserve the existing malformed header identifier response" in {
    withHistory(includeBody = false) { (_, route) =>
      Seq("00", "zz" * 32).foreach { malformed =>
        Get(s"/blocks/$malformed/header") ~> route ~> check {
          status shouldBe StatusCodes.MethodNotAllowed
        }
      }
    }
  }
}
