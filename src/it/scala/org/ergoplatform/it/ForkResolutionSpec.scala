package org.ergoplatform.it

import java.io.File

import cats.implicits._
import com.typesafe.config.Config
import org.ergoplatform.it.container.Docker.{ExtraConfig, noExtraConfig}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
import scala.util.Try

class ForkResolutionSpec extends FreeSpec with IntegrationSuite {

  val nodesQty: Int = 4

  val commonChainLength: Int = 5
  val forkLength: Int = 5
  val syncLength: Int = 15

  val localVolumes: Seq[String] = (1 to nodesQty).map(localVolume)
  val remoteVolume = "/app"

  val volumesMapping: Seq[(String, String)] = localVolumes.map(_ -> remoteVolume)

  val dirs: Seq[File] = localVolumes.map(vol => new File(vol))
  dirs.foreach(_.mkdirs())

  val minerConfig: Config = nodeSeedConfigs.head
  val onlineMiningNodesConfig: List[Config] = nodeSeedConfigs.slice(1, nodesQty)
    .map(_.withFallback(onlineGeneratingPeerConfig))
  val offlineMiningNodesConfig: List[Config] = nodeSeedConfigs.slice(1, nodesQty)

  def localVolume(n: Int): String = s"$localDataDir/fork-resolution-spec/node-$n/data"

  def startNodesWithBinds(nodeConfigs: List[Config],
                          configEnrich: ExtraConfig = noExtraConfig): Try[List[Node]] = {
    log.trace(s"Starting ${nodeConfigs.size} containers")
    val nodes: Try[List[Node]] = nodeConfigs
      .map(_.withFallback(specialDataDirConfig(remoteVolume)))
      .zip(volumesMapping)
      .map { case (cfg, vol) => docker.startDevNetNode(cfg, configEnrich, Some(vol)) }
      .sequence
    blocking(Thread.sleep(nodeConfigs.size * 3000))
    nodes
  }

  // Testing scenario:
  // 1. Start up {nodesQty} nodes and let them mine common chain of length {initialCommonChainLength};
  // 2. Kill all nodes when they are done, make them offline generating, clear `knownPeers` and restart them;
  // 3. Let them mine another {forkLength} blocks offline in order to create {nodesQty} forks;
  // 4. Kill all nodes again and restart with `knownPeers` filled, wait another {syncLength} blocks;
  // 5. Check that nodes reached consensus on created forks;
  "Fork resolution after isolated mining" in {

    val nodes: List[Node] = startNodesWithBinds(minerConfig +: onlineMiningNodesConfig).get

    val result = Async.async {
      val initMaxHeight = Async.await(Future.traverse(nodes)(_.height).map(_.max))
      Async.await(Future.traverse(nodes)(_.waitForHeight(initMaxHeight + commonChainLength)))
      val isolatedNodes = Async.await {
        nodes.foreach(node => docker.stopNode(node.containerId))
        Future.successful(startNodesWithBinds(minerConfig +: offlineMiningNodesConfig, isolatedPeersConfig).get)
      }
      val forkHeight = initMaxHeight + commonChainLength + forkLength
      Async.await(Future.traverse(isolatedNodes)(_.waitForHeight(forkHeight)))
      val regularNodes = Async.await {
        isolatedNodes.foreach(node => docker.stopNode(node.containerId))
        Future.successful(startNodesWithBinds(minerConfig +: onlineMiningNodesConfig).get)
      }
      Async.await(Future.traverse(regularNodes)(_.waitForHeight(forkHeight + syncLength)))
      val headers = Async.await(Future.traverse(regularNodes)(_.headerIdsByHeight(forkHeight)))

      log.debug(s"Headers at height $initMaxHeight: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.map(_.headOption.value)
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }

    Await.result(result, 15.minutes)
  }

}
