package org.ergoplatform.it

import java.io.File
import akka.japi.Option.Some
import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class PrunedDigestNodeSync2Spec extends AnyFlatSpec with IntegrationSuite {

  val approxTargetHeight = 40
  val blocksToKeep: Int = 2
  // mined >= 15 s before the target height: its header is never fresh for the digest node
  val earlyHeight = 10

  val localVolume = s"$localDataDir/digest-node-sync-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val minerConfig: Config = nodeSeedConfigs.head
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(allowLocalConfig)

  val digestConfig: Config = digestStatePeerConfig
    .withFallback(blockIntervalConfig(600))
    .withFallback(headerChainDiffConfig(20))
    .withFallback(prunedHistoryConfig(blocksToKeep))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
    .withFallback(allowLocalConfig)

  // Testing scenario:
  // 1. Start up mining node and let it mine chain of length ~ {approxTargetHeight};
  // 2. Start digest node. It starts to load full blocks near the first header younger
  //    than blockInterval * headerChainDiff = 600 ms * 20 = 12 s;
  // 3. Wait until it gets synced up to {approxTargetHeight}, while the miner keeps mining;
  // 4. Make sure digest node does not store the full block at {earlyHeight};
  it should "Pruned digest node synchronization" in {

    val minerNode: Node = docker.startDevNetNode(minerConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

    val result = Async.async {
      Async.await(minerNode.waitForHeight(approxTargetHeight, 1.second))
      val digestNode = docker.startDevNetNode(digestConfig).get
      Async.await(digestNode.waitForHeight(approxTargetHeight))
      val earlyBlockId = Async.await(minerNode.headerIdsByHeight(earlyHeight)).head
      Async.await(
        digestNode.singleGet(s"/blocks/$earlyBlockId").map(_.getStatusCode)
      ) shouldBe 404
    }

    Await.result(result, 3.minute)
  }

}
