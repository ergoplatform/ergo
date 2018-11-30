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

class PrunedDigestNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val approxTargetHeight = 20
  val blocksToKeep: Int = approxTargetHeight / 5

  val localVolume = s"$localDataDir/digest-node-sync-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(miningDelayConfig(10000))
    .withFallback(specialDataDirConfig(remoteVolume))
  val digestConfig: Config = digestStatePeerConfig
    .withFallback(blockIntervalConfig(8000))
    .withFallback(prunedHistoryConfig(blocksToKeep))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))

  // Testing scenario:
  // 1. Start up mining node and let it mine chain of length ~ {approxTargetHeight};
  // 2. Shut it down, restart with turned off mining and fetch its info to get actual {targetHeight};
  // 3. Start digest node and wait until it gets synced with the first one up to {targetHeight} ensuring
  //    it does not load full block that should be pruned;
  // 4. Fetch digest node info and compare it with first node's one;
  // 5. Make sure digest node does not store full blocks with height < {targetHeight - blocksToKeep};
  "Pruned digest node synchronization" in {

    val minerNode: Node = docker.startNode(minerConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

    val result = Async.async {
      Async.await(minerNode.waitForHeight(approxTargetHeight, 1.second))
      log.info(s"Wait for height $approxTargetHeight - DONE")
      docker.stopNode(minerNode.containerId, secondsToWait = 0)
      val nodeForSyncing = docker.startNode(
        minerConfig.withFallback(nonGeneratingPeerConfig), specialVolumeOpt = Some((localVolume, remoteVolume))).get
      Async.await(nodeForSyncing.waitForHeight(approxTargetHeight))
      log.info(s"Wait for height $approxTargetHeight on restarted node - DONE")
      val sampleInfo = Async.await(nodeForSyncing.info)
      log.info(s"Get sample info - DONE")
      val digestNode = docker.startNode(digestConfig).get
      val targetHeight = sampleInfo.bestBlockHeightOpt.value
      val targetBlockId = sampleInfo.bestBlockIdOpt.value
      val blocksToPrune = Async.await(nodeForSyncing.headers(0, targetHeight - blocksToKeep - 1))
      log.info(s"Get headers to be pruned - DONE")
      Async.await(digestNode.waitFor[Option[String]](
        _.info.map(_.bestHeaderIdOpt),
        headerIdOpt => {
          headerIdOpt.foreach(blocksToPrune should not contain _)
          headerIdOpt.contains(targetBlockId)
        },
        50.millis
      ))
      log.info(s"Wait for target block $targetBlockId - DONE")
      val digestNodeInfo = Async.await(digestNode.info)
      log.info(s"Wait for digest node info - DONE")
      digestNodeInfo shouldEqual sampleInfo
      Async.await(digestNode.singleGet(s"/blocks/${blocksToPrune.last}")
        .map(_.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)) shouldBe false
    }

    Await.result(result, 10.minutes)
  }

}
