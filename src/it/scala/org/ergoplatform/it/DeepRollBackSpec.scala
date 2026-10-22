package org.ergoplatform.it

import java.io.File
import java.util.concurrent.TimeoutException
import com.typesafe.config.Config
import io.circe.Json
import org.ergoplatform.it.api.NodeApi.{NodeInfo, nodeInfoDecoder}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils
import org.ergoplatform.it.util.{ConvergenceObservations, NoProgressException, StallWatch}
import org.scalatest.freespec.AnyFreeSpec
import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DeepRollBackSpec extends AnyFreeSpec with IntegrationSuite {

  val keepVersions = 350
  val chainLength = 50
  val delta = 150
  // sync v2 finds a common point only through the headers 0, 16, 128 and 512 blocks
  // below a node's tip (`ErgoHistoryReader.FullV2SyncOffsets`), so nodeB's header 128
  // blocks below its fork tip must be on the chain it shares with nodeA. That tip is at
  // least `sharedChainLength + chainLength` = 130, and nodeB's fork is under 128 blocks.
  val sharedChainLength = 80

  val localVolumeA = s"$localDataDir/node-rollback-spec/nodeA/data"
  val localVolumeB = s"$localDataDir/node-rollback-spec/nodeB/data"
  val remoteVolumeA = "/appA"
  val remoteVolumeB = "/appB"

  val (dirA, dirB) = (new File(localVolumeA), new File(localVolumeB))
  dirA.mkdirs(); dirB.mkdirs()

  // whenever nodeA mines, it mines alone (nodeB does not mine while it syncs, and the
  // isolated miners have no connection), so it can poll fast, as in LongChainSyncSpec.
  // nodeB polls slower, so its fork stays well under 128 blocks until it stops.
  val minerAConfig: Config = specialDataDirConfig(remoteVolumeA)
    .withFallback(internalMinerPollingIntervalConfig(100))
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

  private val observations = new ConvergenceObservations
  private case class NodeSnapshot(info: Option[NodeInfo])
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
    }
    status.zip(peers).map { case (info, _) => NodeSnapshot(info) }
  }

  private def observeNodes(
    phase: String,
    nodeA: Node,
    nodeB: Node,
    budget: FiniteDuration = 5.seconds
  ): Future[(NodeSnapshot, NodeSnapshot)] = {
    snapshot(phase, "A", nodeA, budget).zip(snapshot(phase, "B", nodeB, budget))
  }

  private def headersAheadOfFullBlocks(info: NodeInfo): Int =
    info.bestHeaderHeightOpt.getOrElse(0) - info.bestBlockHeightOpt.getOrElse(0)

  private def waitForSameBestBlock(
    nodeA: Node,
    nodeB: Node,
    minHeight: Int,
    timeout: FiniteDuration
  ): Future[(NodeInfo, NodeInfo)] = {
    def infos(pair: (NodeSnapshot, NodeSnapshot)): Option[(NodeInfo, NodeInfo)] =
      for (infoA <- pair._1.info; infoB <- pair._2.info) yield (infoA, infoB)
    def converged(pair: (NodeSnapshot, NodeSnapshot)): Boolean = infos(pair).exists {
      case (a, b) => ConvergenceObservations.sameBestBlock(a, b, minHeight)
    }
    // mining is off, so only the switch to the better chain changes either node's /info
    val stall = new StallWatch[(NodeInfo, NodeInfo)]()
    observations.until(timeout.fromNow, 1.second, 5.seconds) { budget =>
      observeNodes("convergence", nodeA, nodeB, budget).flatMap { pair =>
        val quiet = stall.record(infos(pair))
        if (!converged(pair) && quiet >= StallWatch.DefaultLimit) {
          docker.describeNodes(Seq(nodeA, nodeB)).flatMap { nodes =>
            Future.failed(new NoProgressException(
              s"Nodes stopped converging to the same best full block at height >= " +
                s"$minHeight: no tip change observed for ${quiet.toSeconds} s; nodes: " +
                s"$nodes; recent observations: $lastObservation"))
          }
        } else {
          Future.successful(pair)
        }
      }
    }(converged)(
      s"Nodes did not converge to the same best full block at height >= $minHeight; " +
        s"recent observations: $lastObservation"
    ).map { case (a, b) =>
      log.info(s"Converged, longest quiet period ${stall.longestQuietPeriod.toMillis} ms")
      (a.info.get, b.info.get)
    }
  }

  "Deep rollback handling" in {

    val result: Future[Unit] = Async.async {

      // 1. Let nodeA mine a shared chain of `sharedChainLength` blocks and sync nodeB

      val minerAGen: Node = docker.startDevNetNode(minerAConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      val minerBGen: Node = docker.startDevNetNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get

      Async.await(minerAGen.waitForHeight(sharedChainLength))
      Async.await(minerBGen.waitForHeight(sharedChainLength))

      val genesisAGen = Async.await(minerAGen.headerIdsByHeight(ErgoHistoryUtils.GenesisHeight)).head
      val genesisBGen = Async.await(minerBGen.headerIdsByHeight(ErgoHistoryUtils.GenesisHeight)).head

      val minerAGenBestHeight = Async.await(minerAGen.fullHeight)
      val minerBGenBestHeight = Async.await(minerBGen.fullHeight)

      log.info("heightA: " + minerAGenBestHeight)
      log.info("heightB: " + minerBGenBestHeight)

      genesisAGen shouldBe genesisBGen
      Async.await(observeNodes("initial shared chain", minerAGen, minerBGen))

      // nodeB's miner starts in isolation only if its headers are fewer than 6 above its
      // full blocks; 3 leaves room for the headers nodeB still gets while nodeA stops, so
      // `sharedHeight` is only a lower bound of nodeB's fork base
      val lastSharedInfo = Async.await(
        minerBGen.waitForProgress[NodeInfo, Int](
          "nodeB's headers less than 3 ahead of its full blocks",
          _.info,
          headersAheadOfFullBlocks(_) < 3,
          100.millis
        )(_.bestBlockHeightOpt, i => s"headers ahead: ${headersAheadOfFullBlocks(i)}")
      )
      val sharedHeight = lastSharedInfo.bestBlockHeightOpt.value

      // 2. Stop all nodes
      docker.stopNode(minerAGen.containerId)
      docker.stopNode(minerBGen.containerId)
      // `noConnectionsConfig` blocks every connection; removing the peer databases as
      // well is a second guard
      docker.removeFromMountedVolume(localVolumeA, remoteVolumeA, "peers")
      docker.removeFromMountedVolume(localVolumeB, remoteVolumeB, "peers")

      val minerAIsolated: Node = docker.startDevNetNode(minerAConfig, noConnectionsConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      // 1. Let nodeA mine in isolation up to `chainLength + delta` blocks above the
      // shared chain; its lead of at least `delta` (more than 128 blocks) puts nodeB on
      // the far-behind download path when it switches (#2673)
      Async.await(minerAIsolated.waitForHeight(sharedHeight + chainLength + delta))

      val minerBIsolated: Node = docker.startDevNetNode(minerBConfig, noConnectionsConfig,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get
      Async.await(observeNodes("isolated miners started", minerAIsolated, minerBIsolated))
      // in isolation nodeB gets no new headers, so its miner never starts if its headers
      // are 6 or more above its full blocks
      val minerBStartGap = headersAheadOfFullBlocks(Async.await(minerBIsolated.info))
      withClue("nodeB's headers above its full blocks at its isolated start: ") {
        minerBStartGap should be < 6
      }

      // 2. Let nodeB mine `chainLength` blocks in isolation
      Async.await(minerBIsolated.waitForHeight(sharedHeight + chainLength, 100.millis))

      log.info("Mining phase done")

      val minerABestHeight = Async.await(minerAIsolated.fullHeight)
      val minerBBestHeight = Async.await(minerBIsolated.fullHeight)
      Async.await(observeNodes("isolated mining complete", minerAIsolated, minerBIsolated))
      // with no peers each miner mined its own fork: different blocks `chainLength / 2`
      // below nodeB's tip show that nodeB has a deep rollback to do
      val isolatedMiners = Seq(minerAIsolated, minerBIsolated)
      val isolatedPeers = Async.await(Future.traverse(isolatedMiners) { node =>
        node.connectedPeers.map(peers => node.nodeName -> peers.map(_.name))
      })
      isolatedPeers.filter(_._2.nonEmpty) shouldBe empty
      val forkCheckHeight = minerBBestHeight - chainLength / 2
      val forkIds = Async.await(Future.traverse(isolatedMiners) { node =>
        node
          .headerIdsByHeight(forkCheckHeight)
          .map(ids => node.nodeName -> ids.headOption)
      })
      // both nodes hold a block at this height, and the blocks differ
      withClue(s"header ids at height $forkCheckHeight: $forkIds: ") {
        forkIds.flatMap(_._2).distinct should have size 2
      }

      docker.stopNode(minerAIsolated.containerId)
      docker.stopNode(minerBIsolated.containerId)

      log.info("heightA: " + minerABestHeight)
      log.info("heightB: " + minerBBestHeight)

      // the far-behind download path needs a lead of more than 128 blocks
      withClue(s"nodeA at $minerABestHeight, nodeB at $minerBBestHeight: ") {
        minerABestHeight - minerBBestHeight should be > 128
      }

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
        log.error(s"Deep rollback timed out; last observation: $lastObservation")
        throw error
    } finally {
      observations.close()
    }
  }

}
