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

class DigestStateNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val approxTargetHeight = 10
  val blocksToKeep: Int = approxTargetHeight / 2

  val localVolume = s"$localDataDir/digest-node-sync-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(specialDataDirConfig(remoteVolume))
  val digestConfig: Config = digestStatePeerConfig
    .withFallback(prunedHistoryPeerConfig(blocksToKeep))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))

  // Testing scenario:
  // 1. Start up mining node and let it mine chain of length ~ {approxTargetHeight};
  // 2. Shut it down, restart with turned off mining and fetch its info to get actual {targetHeight};
  // 3. Start digest node and wait until it gets synced with the first one up to {targetHeight};
  // 4. Fetch digest node info and compare it with first node's one;
  // 5. Make sure digest node does not store full blocks with height < {targetHeight - blocksToKeep};
  s"Digest mode synchronization ($approxTargetHeight blocks)" in {

    val minerNode: Node = docker.startNode(minerConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

    val result = Async.async {
      Async.await(minerNode.waitForHeight(approxTargetHeight, 500.millis))
      docker.stopNode(minerNode.containerId)
      val nodeForSyncing = docker.startNode(
        minerConfig.withFallback(nonGeneratingPeerConfig), specialVolumeOpt = Some((localVolume, remoteVolume))).get
      val sampleInfo = Async.await(nodeForSyncing.info)
      val digestNode = docker.startNode(digestConfig).get
      val targetHeight = sampleInfo.bestBlockHeightOpt.value
      Async.await(digestNode.waitForHeight(targetHeight))
      val digestNodeInfo = Async.await(digestNode.info)
      digestNodeInfo shouldEqual sampleInfo
      val digestNodePrunedBlockId = Async.await(digestNode.headerIdsByHeight(targetHeight - blocksToKeep - 1)).head
      Async.await(digestNode.singleGet(s"/blocks/$digestNodePrunedBlockId")
        .map(_.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)) shouldBe false
    }

    Await.result(result, 10.minutes)
  }

}
