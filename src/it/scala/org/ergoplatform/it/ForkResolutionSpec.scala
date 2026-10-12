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

  val commonChainLength: Int = 5
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
  // 2. Kill all nodes when they are done, make them offline generating, clear known peers and restart them;
  // 3. Let them mine another {forkLength} blocks offline in order to create {nodesQty} forks;
  // 4. Kill all nodes again and restart with `knownPeers` filled, wait another {syncLength} blocks;
  // 5. Check that nodes reached consensus on created forks;
  it should "Fork resolution after isolated mining" in {

    log.info(minerConfig.toString)
    onlineSyncNodesConfig.foreach(x => log.info(x.toString))

    val nodes: List[Node] = startNodesWithBinds(minerConfig +: onlineSyncNodesConfig)

    val result = Async.async {
      val initMaxHeight = Async.await(Future.traverse(nodes)(_.fullHeight).map(_.max))
      Async.await(Future.traverse(nodes)(_.waitForHeight(initMaxHeight + commonChainLength, 100.millis)))
      val commonHeads = Async.await(Future.traverse(nodes)(_.headerIdsByHeight(initMaxHeight + commonChainLength)))
        .map(_.headOption.value)
      commonHeads.distinct.size shouldBe 1

      // Stop producing the common chain before disconnecting its followers. A follower can
      // have headers ahead of applied blocks; isolated mining cannot fill that gap without peers.
      docker.stopNode(nodes.head.containerId)
      val quiescedMiner = startNodesWithBinds(List(nonGeneratingPeerConfig.withFallback(minerConfig))).head
      val quiescedNodes = quiescedMiner +: nodes.tail
      val commonHeight = initMaxHeight + commonChainLength
      val appliedCommonTip = Async.await(quiescedMiner.waitForProgress(
        "all nodes to share a fully applied tip before isolation",
        _ => Future.traverse(quiescedNodes)(_.info),
        (infos: List[NodeInfo]) => infos.forall { info =>
          info.isMining.contains(false) &&
            info.bestBlockHeightOpt.exists(_ >= commonHeight) &&
            info.bestHeaderHeightOpt == info.bestBlockHeightOpt &&
            info.bestBlockIdOpt.nonEmpty &&
            info.bestHeaderIdOpt == info.bestBlockIdOpt
        } && infos.map(_.bestBlockIdOpt).distinct.size == 1,
        100.millis
      )(
        infos => Some(infos.map(info =>
          (info.bestBlockHeightOpt, info.bestHeaderHeightOpt, info.bestBlockIdOpt, info.bestHeaderIdOpt))),
        infos => infos.map(info =>
          s"full=${info.bestBlockHeightOpt} header=${info.bestHeaderHeightOpt} " +
            s"fullId=${info.bestBlockIdOpt} headerId=${info.bestHeaderIdOpt} mining=${info.isMining}").mkString("; ")
      ))
      log.info(s"Fully applied common tip before isolation: ${appliedCommonTip.head.bestBlockIdOpt.value}")
      val isolatedNodes = Async.await {
        quiescedNodes.foreach(node => docker.stopNode(node.containerId))
        clearPeerDatabases()
        Future.successful(startNodesWithBinds(minerConfig +: offlineMiningNodesConfig, isolatedPeersConfig))
      }
      Async.await(Future.traverse(isolatedNodes)(_.connectedPeers)).foreach(_ shouldBe empty)
      // Shutdown is sequential and the common miner can advance while a slower peer catches up.
      // Choose a height beyond every persisted isolated tip, rather than a height mined before isolation.
      val forkHeight = Async.await(Future.traverse(isolatedNodes)(_.fullHeight)).max + forkLength
      Async.await(Future.traverse(isolatedNodes)(_.waitForHeight(forkHeight, 100.millis)))
      Async.await(Future.traverse(isolatedNodes)(_.connectedPeers)).foreach(_ shouldBe empty)
      val isolatedHeads = Async.await(Future.traverse(isolatedNodes)(_.headerIdsByHeight(forkHeight)))
        .map(_.headOption.value)
      log.info(s"Isolated fork heads at height $forkHeight: ${isolatedHeads.mkString(",")}")
      isolatedHeads.distinct.size shouldBe nodesQty
      val regularNodes = Async.await {
        isolatedNodes.foreach(node => docker.stopNode(node.containerId))
        clearPeerDatabases()
        Future.successful(startNodesWithBinds(minerConfig +: onlineSyncNodesConfig))
      }
      Async.await(Future.traverse(regularNodes)(_.waitForHeight(forkHeight + syncLength, 100.millis)))
      val headers = Async.await(regularNodes.head.waitForProgress[Seq[Seq[String]], Seq[Option[String]]](
        s"all nodes to select the same header at height $forkHeight",
        _ => Future.traverse(regularNodes)(_.headerIdsByHeight(forkHeight)),
        ids => ids.head.headOption.nonEmpty && ids.map(_.headOption).distinct.size == 1,
        100.millis
      )(
        ids => Some(ids.map(_.headOption)),
        ids => s"selected ${ids.map(_.headOption.getOrElse("none")).mkString(",")}"
      ))
      log.info(s"Converged headers at height $forkHeight: ${headers.map(_.headOption.value).mkString(",")}")
    }

    Await.result(result, 15.minutes)
  }

}
