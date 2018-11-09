package org.ergoplatform.it

import java.io.File

import akka.japi.Option.Some
import com.typesafe.config.Config
import org.asynchttpclient.util.HttpConstants
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class PrunedFullNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val approxTargetHeight = 10
  val blocksToKeep: Int = approxTargetHeight / 2

  val localVolume = s"$localDataDir/digest-node-sync-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(miningDelayConfig(10000))
  val prunedConfig: Config = nodeSeedConfigs(1)
    .withFallback(blockIntervalConfig(9500))
    .withFallback(prunedHistoryConfig(blocksToKeep))
    .withFallback(nonGeneratingPeerConfig)

  // Testing scenario:
  // 1. Start up few full nodes and let them mine {targetHeight + syncDelta} blocks;
  // 2. Fetch their headers at {targetHeight} and make sure chains are synced (all nodes have same best headers);
  // 3. Fetch `stateRoot` of best header at {targetHeight};
  // 4. Start up pruned full node and wait until it gets synced with the network up to {targetHeight};
  // 5. Fetch its `stateRoot` of best header at {targetHeight} and make sure it matches full nodes' one;
  s"Pruned full node synchronization ($approxTargetHeight blocks)" in {

    val minerNode: Node = docker.startNode(minerConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

    val result = Async.async {
      Async.await(minerNode.waitForHeight(approxTargetHeight, 500.millis))
      docker.stopNode(minerNode.containerId, secondsToWait = 0)
      val nodeForSyncing = docker.startNode(
        minerConfig.withFallback(nonGeneratingPeerConfig), specialVolumeOpt = Some((localVolume, remoteVolume))).get
      Async.await(nodeForSyncing.waitForHeight(approxTargetHeight))
      val sampleInfo = Async.await(nodeForSyncing.info)
      val prunedNode = docker.startNode(prunedConfig).get
      val targetHeight = sampleInfo.bestBlockHeightOpt.value
      Async.await(prunedNode.waitForHeight(targetHeight))
      val prunedNodeInfo = Async.await(prunedNode.info)
      prunedNodeInfo shouldEqual sampleInfo
      val nodePrunedBlockId = Async.await(prunedNode.headerIdsByHeight(targetHeight - blocksToKeep - 1)).head
      Async.await(prunedNode.singleGet(s"/blocks/$nodePrunedBlockId")
        .map(_.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)) shouldBe false
    }

    Await.result(result, 10.minutes)
  }

}
