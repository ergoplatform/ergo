package org.ergoplatform.it

import java.nio.file.{Files, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import io.circe.parser.parse
import org.ergoplatform.it.container.Docker.noExtraConfig
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

// Node 0 is the sole miner and votes yes; nodes 1-4 are non-mining followers that
// must end up on the same chain. Voting starts at devnet's first epoch boundary
// (height 8); with only the seed casting votes, all 16 Phase-1 blocks come from a
// voter and the rollout crosses the 90% threshold. Mid-scenario enrollment via
// container restart is incompatible with this cadence — a single non-voter block
// during Phase 1 drops the count below threshold and the rollout is cleaned up
// rather than activating.
class SoftForkRolloutSpec extends AnyFlatSpec with Matchers with IntegrationSuite {

  import SoftForkRolloutSpec._

  private val nodesQty = 5

  private val votingLength = 8
  private val softForkEpochs = 2
  private val activationEpochs = 2
  private val phase1Blocks = votingLength * softForkEpochs
  private val activationOffset = votingLength * (softForkEpochs + activationEpochs)
  private val cleanupOffset = votingLength * (softForkEpochs + activationEpochs + 1)
  private val expectedStartingHeight = votingLength
  // Devnet always launches at Interpreter50Version = 3 and the soft-fork bumps to 4.
  private val preActivationBlockVersion = 3
  private val postActivationBlockVersion = 4

  // networkType=devnet is essential: application.conf defaults to testnet, which
  // launches with BlockVersion=4 and leaves protocolVersion (also 4) unable to
  // satisfy `protocolVersion > header.version` in CandidateGenerator.forkOrdered,
  // so no voter would ever cast a vote. Devnet launches with BlockVersion=3.
  // internalMinerPollingInterval is the devnet template default's main slow knob —
  // drop it from 5s to 500ms so the scenario completes in ~1 minute instead of ~5.
  private val votingOverrideConfig: Config = ConfigFactory.parseString(
    s"""
       |ergo.networkType = "devnet"
       |ergo.node.internalMinerPollingInterval = 500ms
       |ergo.chain.voting.votingLength = $votingLength
       |ergo.chain.voting.softForkEpochs = $softForkEpochs
       |ergo.chain.voting.activationEpochs = $activationEpochs
       |""".stripMargin
  )

  private val nonGeneratingConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining = false
      |""".stripMargin
  )

  // voting.120 = 1 and rulesToDisable live in devnetTemplate.conf (file-loaded), so
  // every container votes yes by default. Jackson's flat-properties serialization
  // mangles arrays passed via -D options, so we cannot inject voting config that way.
  //
  // Only node 0 mines (with offlineGeneration=true) so it can carry the chain alone;
  // nodes 1-4 are passive followers. With fast block production (500ms polling), a
  // multi-miner network can't gossip fast enough to avoid persistent forks. The
  // consensus check at the end still verifies that all followers (mining=false) agree
  // with the seed miner on the canonical chain — that's the witness behaviour we
  // care about.
  private val baseConfigs: List[Config] = nodeSeedConfigs.take(nodesQty).zipWithIndex.map {
    case (cfg, idx) =>
      val withCommon = cfg.withFallback(votingOverrideConfig).withFallback(localOnlyConfig)
      if (idx == 0) withCommon
      else withCommon.withFallback(nonGeneratingConfig)
  }

  private val remoteVolume = "/app"
  private val localVolumes: Seq[String] = (0 until nodesQty).map(i =>
    s"$localDataDir/soft-fork-rollout-spec/node-$i/data"
  )
  private val volumeMapping: Seq[(String, String)] = localVolumes.map(_ -> remoteVolume)
  localVolumes.foreach(v => Files.createDirectories(Paths.get(v)))

  private def fetchParameters(node: Node): Future[Json] = node.get("/info").map { r =>
    parse(r.getResponseBody)
      .getOrElse(Json.Null)
      .hcursor.downField("parameters").as[Json].getOrElse(Json.Null)
  }

  // Polls until the node has applied the epoch boundary at `paramsHeight`. Returns the
  // `parameters` JSON for that boundary. Reading at the boundary itself (rather than
  // waiting for the chain to advance further) avoids reading the next epoch's tally.
  private def fetchParamsAtHeight(node: Node, paramsHeight: Int): Future[Json] =
    node.waitFor[Json](
      n => fetchParameters(n.asInstanceOf[Node]),
      json => json.hcursor.downField("height").as[Int].getOrElse(-1) >= paramsHeight,
      200.millis
    )

  private val events = mutable.Buffer.empty[ScenarioEvent]
  private def emit(e: ScenarioEvent): Unit = {
    log.info(s"[scenario] $e")
    events += e
  }

  private def withDataDir(cfg: Config): Config =
    cfg.withFallback(specialDataDirConfig(remoteVolume))

  private def startAll(): Vector[Node] = {
    val started = baseConfigs.zip(volumeMapping).map { case (cfg, vol) =>
      docker.startDevNetNode(withDataDir(cfg), noExtraConfig, Some(vol)).get
    }.toVector
    Await.result(Future.traverse(started.toList)(_.waitForStartup), 180.seconds)
    started
  }

  private def waitAllToHeight(nodes: Vector[Node], h: Int, timeout: FiniteDuration = 10.minutes): Unit =
    Await.result(Future.traverse(nodes.toList)(_.waitForHeight(h)), timeout)

  // Per-checkpoint waits use only the seed miner so the chain doesn't race past the
  // epoch boundary while a slower node (witness) is still catching up — fetched
  // parameters would otherwise reflect the next epoch's tally rather than this one.
  // waitAllToHeight is reserved for the final cross-node consistency check.
  it should "drive a deterministic soft-fork rollout to activation" in {
    val nodes = startAll()
    val seed = nodes(0)

    Await.result(seed.waitForHeight(expectedStartingHeight - 1), 1.minute)
    emit(BaselineReached(nodesQty))

    val params8 = Await.result(fetchParamsAtHeight(seed, expectedStartingHeight), 1.minute)
    params8.hcursor.downField("softForkStartingHeight").as[Int].toOption shouldBe Some(expectedStartingHeight)
    emit(VotingStarted(expectedStartingHeight))

    val epoch1Height = expectedStartingHeight + votingLength
    val params16 = Await.result(fetchParamsAtHeight(seed, epoch1Height), 1.minute)
    params16.hcursor.downField("softForkVotesCollected").as[Int].toOption shouldBe Some(votingLength)
    emit(EpochTallied(1, votingLength))

    val epoch2Height = expectedStartingHeight + phase1Blocks
    val params24 = Await.result(fetchParamsAtHeight(seed, epoch2Height), 1.minute)
    params24.hcursor.downField("softForkVotesCollected").as[Int].toOption shouldBe Some(phase1Blocks)
    emit(EpochTallied(2, phase1Blocks))

    val activationHeight = expectedStartingHeight + activationOffset
    val paramsActivation = Await.result(fetchParamsAtHeight(seed, activationHeight), 1.minute)
    paramsActivation.hcursor.downField("blockVersion").as[Int].toOption shouldBe Some(postActivationBlockVersion)
    emit(ActivationDetected(preActivationBlockVersion, postActivationBlockVersion))

    val tailHeight = expectedStartingHeight + cleanupOffset
    waitAllToHeight(nodes, tailHeight)
    val headerIds = Await.result(
      Future.traverse(nodes.toList)(_.headerIdsByHeight(tailHeight)), 30.seconds
    )
    val firstIds = headerIds.map(_.headOption.value)
    firstIds.distinct should have size 1
    emit(ChainConsistent(tailHeight))

    val expected = List(
      BaselineReached(nodesQty),
      VotingStarted(expectedStartingHeight),
      EpochTallied(1, votingLength),
      EpochTallied(2, phase1Blocks),
      ActivationDetected(preActivationBlockVersion, postActivationBlockVersion),
      ChainConsistent(tailHeight)
    )
    events.toList shouldBe expected
  }

}

object SoftForkRolloutSpec {

  sealed trait ScenarioEvent
  final case class BaselineReached(nodeCount: Int) extends ScenarioEvent
  final case class VotingStarted(startingHeight: Int) extends ScenarioEvent
  final case class EpochTallied(epochIdx: Int, votesCollected: Int) extends ScenarioEvent
  final case class ActivationDetected(oldVersion: Int, newVersion: Int) extends ScenarioEvent
  final case class ChainConsistent(height: Int) extends ScenarioEvent

}
