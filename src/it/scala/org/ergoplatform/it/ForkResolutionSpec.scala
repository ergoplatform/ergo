package org.ergoplatform.it

import java.io.File
import cats.implicits._
import com.typesafe.config.Config
import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.ergoplatform.it.container.Docker.{ExtraConfig, noExtraConfig}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.async.Async
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

class ForkResolutionSpec extends AnyFlatSpec with Matchers with IntegrationSuite with Eventually {

  val nodesQty: Int = 4

  // sync v2 sends headers at offsets 0, 16, 128, 512 from the tip, so the common chain
  // must be long enough for the header 16 below a forked tip to exist and be common
  val commonChainLength: Int = 16
  val forkLength: Int = 5
  val syncLength: Int = 15

  val localVolumes: Seq[String] = (1 to nodesQty).map(localVolume)
  val remoteVolume = "/app"

  val volumesMapping: Seq[(String, String)] = localVolumes.map(_ -> remoteVolume)

  val dirs: Seq[File] = localVolumes.map(vol => new File(vol))
  dirs.foreach(_.mkdirs())

  val miningTimingConfig: Config = shortInternalMinerPollingInterval
    .withFallback(blockIntervalConfig(500))

  val nodeConfigs: List[Config] = nodeSeedConfigs.take(4)
    .map(_.withFallback(allowLocalConfig).withFallback(miningTimingConfig))

  val minerConfig: Config = nodeConfigs.head
  val onlineSyncNodesConfig: List[Config] = nodeConfigs.slice(1, nodesQty)
    .map(_.withFallback(nonGeneratingPeerConfig))
  val offlineMiningNodesConfig: List[Config] = nodeConfigs.slice(1, nodesQty)

  def localVolume(n: Int): String = s"$localDataDir/fork-resolution-spec/node-$n/data"

  def clearPeerDatabases(): Unit = {
    volumesMapping.foreach { case (localVolume, remoteVolume) =>
      docker.removeFromMountedVolume(localVolume, remoteVolume, "peers")
    }
  }

  def startNodesWithBinds(nodeConfigs: List[Config],
                          configEnrich: ExtraConfig = noExtraConfig): List[Node] = {
    log.trace(s"Starting ${nodeConfigs.size} containers")
    val nodes: Try[List[Node]] = nodeConfigs
      .map(_.withFallback(specialDataDirConfig(remoteVolume)))
      .zip(volumesMapping)
      .map { case (cfg, vol) => docker.startDevNetNode(cfg, configEnrich, Some(vol)) }
      .sequence
    implicit val patienceConfig: PatienceConfig = PatienceConfig((nodeConfigs.size * 2).seconds, 3.second)
    eventually {
      Await.result(Future.traverse(nodes.get)(_.waitForStartup), 180.seconds)
    }
  }

  // Testing scenario:
  // 1. Start up {nodesQty} nodes and let them mine common chain of length {initialCommonChainLength};
  // 2. Stop all nodes (a clean shutdown) when they are done, make them offline generating,
  //    clear known peers and restart them;
  // 3. Let them mine offline until each has its own block {forkLength} above the followers'
  //    headers, so that every follower has a fork;
  // 4. Stop all nodes again and restart with `knownPeers` filled, wait another {syncLength} blocks;
  // 5. Check that nodes reached consensus on created forks;
  it should "Fork resolution after isolated mining" in {

    log.info(minerConfig.toString)
    onlineSyncNodesConfig.foreach(x => log.info(x.toString))

    val nodes: List[Node] = startNodesWithBinds(minerConfig +: onlineSyncNodesConfig)

    val result = Async.async {
      val initMaxHeight = Async.await(Future.traverse(nodes)(_.fullHeight).map(_.max))
      Async.await(Future.traverse(nodes)(_.waitForHeight(initMaxHeight + commonChainLength, 100.millis)))
      // Isolated followers can't mine while their headers are 6+ blocks ahead of full blocks
      val followerInfos = Async.await(nodes.head.waitForProgress[Seq[NodeInfo], Int](
        "followers' headers less than 3 ahead of their full blocks",
        _ => Future.traverse(nodes.tail)(_.info),
        _.forall(i => i.bestHeaderHeightOpt.getOrElse(0) - i.bestBlockHeightOpt.getOrElse(0) < 3),
        100.millis
      )(
        // a follower whose block download stops keeps the minimum still
        infos => Some(infos.map(_.bestBlockHeightOpt.getOrElse(0)).min),
        infos => nodes.tail.zip(infos).map { case (n, i) =>
          s"${n.nodeName} headers=${i.bestHeaderHeightOpt.getOrElse(0)} full=${i.bestBlockHeightOpt.getOrElse(0)}"
        }.mkString(", ")
      ))
      // the followers know no header at forkHeight, so each of them mines its own block there
      val forkHeight = followerInfos.map(_.bestHeaderHeightOpt.getOrElse(0)).max + forkLength
      val isolatedNodes = Async.await {
        nodes.foreach(node => docker.stopNode(node.containerId))
        clearPeerDatabases()
        Future.successful(startNodesWithBinds(minerConfig +: offlineMiningNodesConfig, isolatedPeersConfig))
      }
      Async.await(Future.traverse(isolatedNodes)(_.waitForHeight(forkHeight, 100.millis)))
      // no follower has node01's block at forkHeight, so every follower has a fork to resolve
      val isolatedIds =
        Async.await(Future.traverse(isolatedNodes)(_.headerIdsByHeight(forkHeight).map(_.headOption)))
      isolatedIds.tail should not contain isolatedIds.head
      val regularNodes = Async.await {
        isolatedNodes.foreach(node => docker.stopNode(node.containerId))
        clearPeerDatabases()
        Future.successful(startNodesWithBinds(minerConfig +: onlineSyncNodesConfig))
      }
      Async.await(Future.traverse(regularNodes)(_.waitForHeight(forkHeight + syncLength, 100.millis)))
      val sample = Async.await(regularNodes.head.headerIdsByHeight(forkHeight)).headOption.value
      val headers = Async.await(Future.traverse(regularNodes) { node =>
        node.waitForProgress[Seq[String], String](
          s"header $sample at height $forkHeight",
          _.headerIdsByHeight(forkHeight),
          _.headOption.contains(sample),
          100.millis
        )(_.headOption, ids => s"selected ${ids.headOption.getOrElse("none")}")
      })

      log.debug(s"Headers at height $forkHeight: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.map(_.headOption.value)
      headerIdsAtSameHeight should contain only sample
    }

    Await.result(result, 15.minutes)
  }

}
