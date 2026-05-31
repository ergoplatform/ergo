package org.ergoplatform.http.routes

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.pattern.ask
import akka.testkit.TestDuration
import akka.util.Timeout
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.Version
import org.ergoplatform.http.api.InfoApiRoute
import org.ergoplatform.local.ErgoStatsCollector.NodeInfo.difficultyEncoder
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import org.ergoplatform.local.ErgoStatsCollectorRef
import org.ergoplatform.mining.difficulty.DifficultySerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, Rollback}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Difficulty
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class InfoApiRoutesSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._

  implicit val actorTimeout: Timeout = Timeout(15.seconds.dilated)
  implicit val routeTimeout: RouteTestTimeout = RouteTestTimeout(15.seconds.dilated)
  val statsCollector: ActorRef = ErgoStatsCollectorRef(nodeViewRef, networkControllerRef, null, settings)
  val route: Route = InfoApiRoute(statsCollector, settings.scorexSettings.restApi).route
  val requiredDifficulty = BigInt(1)

  override def beforeAll: Unit = {
    Await.ready(initDifficulty(requiredDifficulty), actorTimeout.duration)
  }

  it should "return info" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      log.info(s"Received node info: $json")
      val c = json.hcursor
      c.downField("name").as[String] shouldEqual Right(settings.scorexSettings.network.nodeName)
      c.downField("appVersion").as[String] shouldEqual Right(Version.VersionString)
      c.downField("stateType").as[String] shouldEqual Right(settings.nodeSettings.stateType.stateTypeName)
      c.downField("isMining").as[Boolean] shouldEqual Right(settings.nodeSettings.mining)
      (System.currentTimeMillis() - c.downField("launchTime").as[Long].toOption.getOrElse(0L)) < 2000 shouldBe true
      c.downField("eip27Supported").as[Boolean] shouldEqual Right(true)
      c.downField("restApiUrl").as[String] shouldEqual Right("https://example.com:80")
    }
  }

  it should "should return non-exponential difficulty in json response" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      val res = json.toString
      log.info(s"Received node info: $res")
      res should include regex """\"difficulty\" : \d+,"""
    }
  }

  "difficulty" should "be encoded with non-exponential form " in {
    val res = difficultyEncoder(requiredDifficulty)
    res.toString shouldEqual requiredDifficulty.toString
  }

  it should "expose a recent rollback via /info/rollbacks" in {
    val branchPoint =
      ModifierId @@ "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
    statsCollector ! Rollback(branchPoint, Some(706158), 2, 3, System.currentTimeMillis())
    Get("/info/rollbacks") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val arr = responseAs[Json].asArray.get
      arr should not be empty
      val first = arr.head.hcursor
      first.downField("branchPointId").as[String] shouldEqual Right(branchPoint: String)
      first.downField("branchPointHeight").as[Int] shouldEqual Right(706158)
      first.downField("depth").as[Int] shouldEqual Right(2)
      first.downField("appliedBlocks").as[Int] shouldEqual Right(3)
    }
  }

  it should "cap recent rollbacks at 20 and order them newest first" in {
    (1 to 22).foreach { i =>
      val id = ModifierId @@ "%064d".format(i)
      statsCollector ! Rollback(id, Some(i), i, i, System.currentTimeMillis())
    }
    Get("/info/rollbacks") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val arr = responseAs[Json].asArray.get
      arr.size shouldBe 20
      arr.head.hcursor.downField("branchPointHeight").as[Int] shouldEqual Right(22)
      arr.last.hcursor.downField("branchPointHeight").as[Int] shouldEqual Right(3)
    }
  }

  private def initDifficulty(difficulty: Difficulty): Future[Option[Difficulty]] = {
    val emptyHistory = generateHistory(
      verifyTransactions = settings.nodeSettings.verifyTransactions,
      stateType = settings.nodeSettings.stateType,
      poPoWBootstrap = settings.nodeSettings.nipopowSettings.nipopowBootstrap,
      blocksToKeep = settings.nodeSettings.blocksToKeep
    )
    val nBits = DifficultySerializer.encodeCompactBits(difficulty)
    val chain = genChain(height = 5, emptyHistory, Header.InitialVersion, nBits)
    val history = applyChain(emptyHistory, chain)
    val generatedDifficulty = history.bestFullBlockOpt
      .map(_.header.requiredDifficulty)
      .map(difficultyEncoder.apply)
    log.info(s"Generated difficulty: $generatedDifficulty")
    statsCollector ! ChangedHistory(history)
    (statsCollector ? GetNodeInfo).mapTo[NodeInfo].map { nodeInfo =>
      val difficulty = nodeInfo.bestFullBlockOpt.map(_.header.requiredDifficulty)
      log.info(s"Set difficulty to: $difficulty")
      difficulty
    }
  }

}

