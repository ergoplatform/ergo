package org.ergoplatform.it

import java.io.File
import java.util.concurrent.TimeoutException
import com.typesafe.config.{Config, ConfigFactory}
import io.circe.Json
import org.ergoplatform.it.api.NodeApi.{NodeInfo, nodeInfoDecoder}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.container.Docker.ExtraConfig
import org.ergoplatform.nodeView.history.ErgoHistoryUtils
import org.ergoplatform.it.util.ConvergenceObservations
import org.scalatest.freespec.AnyFreeSpec
import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DeepRollBackSpec extends AnyFreeSpec with IntegrationSuite {

  val keepVersions = 350
  val chainLength = 50
  val delta = 150

  val localVolumeA = s"$localDataDir/node-rollback-spec/nodeA/data"
  val localVolumeB = s"$localDataDir/node-rollback-spec/nodeB/data"
  val remoteVolumeA = "/appA"
  val remoteVolumeB = "/appB"

  val (dirA, dirB) = (new File(localVolumeA), new File(localVolumeB))
  dirA.mkdirs(); dirB.mkdirs()

  val minerAConfig: Config = specialDataDirConfig(remoteVolumeA)
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.head)
    .withFallback(allowLocalConfig)

  val minerBConfig: Config = specialDataDirConfig(remoteVolumeB)
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.last)
    .withFallback(allowLocalConfig)

  val minerAConfigNonGen: Config = minerAConfig
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(allowLocalConfig)

  val minerBConfigNonGen: Config = minerBConfig
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(allowLocalConfig)

  private def clearPeerDatabases(): Unit = {
    Seq(localVolumeA -> remoteVolumeA, localVolumeB -> remoteVolumeB).foreach {
      case (localVolume, remoteVolume) =>
        docker.removeFromMountedVolume(localVolume, remoteVolume, "peers")
    }
  }

  private val observations = new ConvergenceObservations
  private case class NodeSnapshot(info: Option[NodeInfo], connectedPeers: Option[Int])
  private val probes = scala.collection.mutable.Map.empty[
    Node, (observations.Probe[NodeInfo], observations.Probe[Int])]

  @volatile private var lastObservation = "No node pair observed"
  private var recentObservations = Vector.empty[String]

  private def remember(phase: String, label: String, endpoint: String, summary: String): Unit = synchronized {
    val observation = s"$phase $label $endpoint sampledAt=${System.currentTimeMillis()} $summary"
    recentObservations = (recentObservations :+ observation).takeRight(12)
    lastObservation = recentObservations.mkString("; ")
    log.info(observation)
  }

  private def snapshot(phase: String, label: String, node: Node, budget: FiniteDuration): Future[NodeSnapshot] = {
    val (statusProbe, peerProbe) = probes.getOrElseUpdate(node, (
      observations.probe(node.singleGet("/info", _.setRequestTimeout(5000))
        .map { r =>
          require(r.getStatusCode == 200, "Unexpected observation status")
          node.ergoJsonAnswerAs[NodeInfo](r.getResponseBody)
        }),
      observations.probe(node.singleGet("/peers/connected", _.setRequestTimeout(5000)).map { r =>
        require(r.getStatusCode == 200, "Unexpected observation status")
        node.ergoJsonAnswerAs[Json](r.getResponseBody).asArray.getOrElse(
          throw new IllegalArgumentException("Expected peer array")).size
      })
    ))
    val status = statusProbe.sample(budget).map {
      case Right(info) =>
        val summary = Json.obj(
          "headersHeight" -> info.bestHeaderHeightOpt.map(Json.fromInt).getOrElse(Json.Null),
          "fullHeight" -> info.bestBlockHeightOpt.map(Json.fromInt).getOrElse(Json.Null),
          "bestHeaderId" -> info.bestHeaderIdOpt.map(ConvergenceObservations.headerId).map(Json.fromString).getOrElse(Json.Null),
          "bestFullHeaderId" -> info.bestBlockIdOpt.map(ConvergenceObservations.headerId).map(Json.fromString).getOrElse(Json.Null)
        )
        remember(phase, label, "status", summary.noSpaces)
        Some(info)
      case Left(error) =>
        remember(phase, label, "status", s"errorClass=$error")
        None
    }
    val peers = peerProbe.sample(budget).map { result =>
      val summary = result.fold(error => s"errorClass=$error", count => s"connectedPeerCount=$count")
      remember(phase, label, "peers", summary)
      result.toOption
    }
    status.zip(peers).map { case (info, count) => NodeSnapshot(info, count) }
  }

  private def observeNodes(
    phase: String,
    nodeA: Node,
    nodeB: Node,
    budget: FiniteDuration = 5.seconds
  ): Future[(NodeSnapshot, NodeSnapshot)] = {
    snapshot(phase, "A", nodeA, budget).zip(snapshot(phase, "B", nodeB, budget))
  }

  private def waitForNoPeers(phase: String, nodes: Seq[(String, Node)]): Future[Unit] = {
    observations.until(30.seconds.fromNow, 1.second, 5.seconds) { budget =>
      Future.traverse(nodes) { case (label, node) =>
        snapshot(phase, label, node, budget).map { observed =>
          require(observed.connectedPeers.forall(_ == 0),
            s"Isolated miner $label has connected peers; recent observations: $lastObservation")
          observed
        }
      }
    }(_.forall(_.connectedPeers.contains(0)))(
      s"Isolated miners did not report zero connected peers; recent observations: $lastObservation"
    ).map(_ => ())
  }

  private val seedObservations = new ConvergenceObservations
  @volatile private var lastSeedObservation = "Initial seed has not been sampled"

  private def waitForSettledSeed(
    nodeA: Node,
    nodeB: Node,
    timeout: FiniteDuration
  ): Future[(NodeInfo, NodeInfo)] = {
    def infoProbe(node: Node): seedObservations.Probe[NodeInfo] =
      seedObservations.probe(node.singleGet("/info", _.setRequestTimeout(5000)).map { response =>
        require(response.getStatusCode == 200, "Unexpected seed observation status")
        node.ergoJsonAnswerAs[NodeInfo](response.getResponseBody)
      })

    val probeA = infoProbe(nodeA)
    val probeB = infoProbe(nodeB)
    def describe(result: Either[String, NodeInfo]): String = result.fold(
      error => s"errorClass=$error",
      info => s"headersHeight=${info.bestHeaderHeightOpt}; fullHeight=${info.bestBlockHeightOpt}; " +
        s"headerId=${info.bestHeaderIdOpt.map(ConvergenceObservations.headerId)}; " +
        s"fullId=${info.bestBlockIdOpt.map(ConvergenceObservations.headerId)}; mining=${info.isMining}")
    seedObservations.until(timeout.fromNow, 1.second, 5.seconds) { budget =>
      probeA.sample(budget).zip(probeB.sample(budget)).map { pair =>
        lastSeedObservation = s"A=${describe(pair._1)}; B=${describe(pair._2)}"
        log.info(s"Initial shared-chain readiness: $lastSeedObservation")
        pair
      }
    } {
      case (Right(a), Right(b)) =>
        ConvergenceObservations.sameFullyAppliedNonMiningBlock(a, b, ErgoHistoryUtils.GenesisHeight)
      case _ => false
    }(
      s"Initial chain did not settle with mining disabled and matching full/header tips; $lastSeedObservation"
    ).map { case (a, b) => (a.toOption.get, b.toOption.get) }
  }

  private def waitForSameBestBlock(
    nodeA: Node,
    nodeB: Node,
    minHeight: Int,
    timeout: FiniteDuration
  ): Future[(NodeInfo, NodeInfo)] = {
    observations.until(timeout.fromNow, 1.second, 5.seconds)(
      budget => observeNodes("convergence", nodeA, nodeB, budget)
    ) { case (a, b) =>
      a.info.exists(infoA => b.info.exists(infoB => ConvergenceObservations.sameBestBlock(infoA, infoB, minHeight)))
    }(
      s"Nodes did not converge to the same best full block at height >= $minHeight; " +
        s"recent observations: $lastObservation"
    ).map { case (a, b) => (a.info.get, b.info.get) }
  }

  "Deep rollback handling" in {

    val result: Future[Unit] = Async.async {

      // 1. Let nodeA mine and sync nodeB

      val minerAGen: Node = docker.startDevNetNode(minerAConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      val minerBGen: Node = docker.startDevNetNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get

      Async.await(minerAGen.waitForHeight(1))
      Async.await(minerBGen.waitForHeight(1))

      val genesisAGen = Async.await(minerAGen.headerIdsByHeight(ErgoHistoryUtils.GenesisHeight)).head
      val genesisBGen = Async.await(minerBGen.headerIdsByHeight(ErgoHistoryUtils.GenesisHeight)).head

      genesisAGen shouldBe genesisBGen

      // Freeze the producer while B can still retrieve every header's full block.
      docker.stopNode(minerAGen.containerId)
      val minerASeed: Node = docker.startDevNetNode(minerAConfigNonGen,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get
      val (seedA, seedB) = Async.await(waitForSettledSeed(minerASeed, minerBGen, 2.minutes))
      val seedHeight = seedA.bestBlockHeightOpt.get
      require(seedHeight < chainLength,
        s"Initial shared chain already reached $seedHeight; isolated node B must mine to $chainLength")
      log.info(s"Settled shared chain: heightA=$seedHeight, heightB=${seedB.bestBlockHeightOpt.get}")
      Async.await(observeNodes("initial shared chain", minerASeed, minerBGen))

      // 2. Stop the restarted A and B only after both have the complete shared seed.
      docker.stopNode(minerASeed.containerId)
      docker.stopNode(minerBGen.containerId)
      clearPeerDatabases()

      val minerAIsolated: Node = docker.startDevNetNode(minerAConfig, DeepRollBackSpec.isolatedMiningConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get
      Async.await(waitForNoPeers("isolated miners started", Seq("A" -> minerAIsolated)))

      // 1. Let nodeA mine `chainLength + delta` blocks in isolation
      Async.await(minerAIsolated.waitForHeight(chainLength + delta))

      val minerBIsolated: Node = docker.startDevNetNode(minerBConfig, DeepRollBackSpec.isolatedMiningConfig,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get
      Async.await(waitForNoPeers("isolated miners started",
        Seq("A" -> minerAIsolated, "B" -> minerBIsolated)))

      // 2. Let nodeB mine `chainLength` blocks in isolation
      Async.await(minerBIsolated.waitForHeight(chainLength, 100.millis))

      log.info("Mining phase done")

      val minerABestHeight = Async.await(minerAIsolated.fullHeight)
      val minerBBestHeight = Async.await(minerBIsolated.fullHeight)
      Async.await(waitForNoPeers("isolated mining complete",
        Seq("A" -> minerAIsolated, "B" -> minerBIsolated)))

      docker.stopNode(minerAIsolated.containerId)
      docker.stopNode(minerBIsolated.containerId)
      clearPeerDatabases()

      log.info("heightA: " + minerABestHeight)
      log.info("heightB: " + minerBBestHeight)

      (minerABestHeight > minerBBestHeight) shouldBe true

      // 3. Restart nodeA and nodeB (having shorter chain) with disabled mining
      val minerA: Node = docker.startDevNetNode(minerAConfigNonGen,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      val minerB: Node = docker.startDevNetNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get
      Async.await(observeNodes("restarted without mining", minerA, minerB))

      val isMiningAOpt = Async.await(minerA.info).isMining
      log.info("isminingA: " + isMiningAOpt)
      isMiningAOpt map (_ shouldBe false)

      val isMiningBOpt = Async.await(minerB.info).isMining
      log.info("isminingB: " + isMiningBOpt)
      isMiningBOpt map (_ shouldBe false)

      // 5. Wait until it switches to the better chain
      val (minerAInfo, minerBInfo) =
        Async.await(waitForSameBestBlock(minerA, minerB, minerABestHeight, 10.minutes))

      log.info("Chain switching done")

      minerBInfo.bestBlockIdOpt shouldEqual minerAInfo.bestBlockIdOpt
    }

    try {
      Await.result(result, 20.minutes)
    } catch {
      case error: TimeoutException =>
        log.error(s"Deep rollback timed out; last initial-seed observation: $lastSeedObservation; " +
          s"last phase observation: $lastObservation")
        throw error
    } finally {
      try observations.close()
      finally seedObservations.close()
    }
  }

}

private[it] object DeepRollBackSpec {
  val isolatedMiningConfig: ExtraConfig = { (_, _) =>
    Some(ConfigFactory.parseString("scorex.network { knownPeers = [], maxConnections = 0 }"))
  }
}
