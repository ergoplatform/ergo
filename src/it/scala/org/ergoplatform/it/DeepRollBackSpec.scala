package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DeepRollBackSpec extends FreeSpec with IntegrationSuite {

  val keepVersions = 350
  val chainLength = 200
  val delta = 100

  val localVolumeA = s"$localDataDir/node-rollback-spec/nodeA/data"
  val localVolumeB = s"$localDataDir/node-rollback-spec/nodeB/data"
  val remoteVolume = "/app"

  val (dirA, dirB) = (new File(localVolumeA), new File(localVolumeB))
  dirA.mkdirs(); dirB.mkdirs()

  val minerAConfig: Config = specialDataDirConfig(remoteVolume)
    .withFallback(shortMiningDelayConfig)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.head)
  val minerBConfig: Config = specialDataDirConfig(remoteVolume)
    .withFallback(shortMiningDelayConfig)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.last)

  val minerAConfigNonGen: Config = minerAConfig
    .withFallback(nonGeneratingPeerConfig)
  val minerBConfigNonGen: Config = minerBConfig
    .withFallback(nonGeneratingPeerConfig)

  "Deep rollback handling" in {

    val result: Future[Unit] = Async.async {

      val minerAIsolated: Node = docker.startTestNetNode(minerAConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolume))).get

      // 1. Let the first node mine `chainLength + delta` blocks
      Async.await(minerAIsolated.waitForHeight(chainLength + delta))

      val genesisA = Async.await(minerAIsolated.headerIdsByHeight(ErgoHistory.GenesisHeight)).head

      val minerBIsolated: Node = docker.startTestNetNode(minerBConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeB, remoteVolume))).get

      // 2. Let another node mine `chainLength` blocks
      Async.await(minerBIsolated.waitForHeight(chainLength))

      val genesisB = Async.await(minerBIsolated.headerIdsByHeight(ErgoHistory.GenesisHeight)).head

      genesisA should not equal genesisB

      docker.stopNode(minerBIsolated.containerId)

      log.info("Mining phase done")

      val minerABestHeight = Async.await(minerAIsolated.height)

      // 3. Restart node B with disabled mining (it has shorter chain)
      val minerB: Node = docker.startTestNetNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolume))).get

      // 5. Wait until it switches to the better chain
      Async.await(minerB.waitForHeight(minerABestHeight))

      log.info("Chain switching done")

      val minerABestBlock = Async.await(minerAIsolated.headerIdsByHeight(minerABestHeight)).head
      val minerBBestBlock = Async.await(minerB.headerIdsByHeight(minerABestHeight)).head

      minerBBestBlock shouldEqual minerABestBlock
    }

    Await.result(result, 15.minutes)

  }

}
