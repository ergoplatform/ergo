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

class PrunedDigestNodeSync2Spec extends FreeSpec with IntegrationSuite {

  val approxTargetHeight = 100
  val blocksToKeep: Int = 2

  val localVolume = s"$localDataDir/digest-node-sync-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(miningDelayConfig(1000))
    .withFallback(specialDataDirConfig(remoteVolume))
  val nodeForSyncingConfig: Config = minerConfig
    .withFallback(nonGeneratingPeerConfig)
  val digestConfig: Config = digestStatePeerConfig
    .withFallback(blockIntervalConfig(600))
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

    val minerNode: Node = docker.startDevNetNode(minerConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

    val result = Async.async {
      Async.await(minerNode.waitForHeight(approxTargetHeight, 20.second))
//      docker.stopNode(minerNode, secondsToWait = 0)
      val digestNode = docker.startDevNetNode(digestConfig).get
      Async.await(digestNode.waitForHeight(approxTargetHeight))
      Async.await(digestNode.info)
    }

    Await.result(result, 3.minute)
  }

}
