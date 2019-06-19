package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DeepRollBackSpec extends FreeSpec with IntegrationSuite {

  val keepVersions = 300
  val chainLength = 250
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
      val minerAIsolated: Node = docker.startNode(minerAConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolume))).get

      // 1. Let the first node mine `chainLength + delta` blocks
      Async.await(minerAIsolated.waitForHeight(chainLength + delta))
      docker.stopNode(minerAIsolated.containerId)

      val minerBIsolated: Node = docker.startNode(minerBConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeB, remoteVolume))).get

      // 2. Let another node mine `chainLength` blocks
      Async.await(minerBIsolated.waitForHeight(chainLength))
      docker.stopNode(minerBIsolated.containerId)

      log.info("Mining phase done")

      // 3. Restart the first node with disabled mining
      val minerA: Node = docker.startNode(minerAConfigNonGen,
        specialVolumeOpt = Some((localVolumeA, remoteVolume))).get
      Async.await(minerA.waitForHeight(chainLength + delta))

      val minerABestHeight = Async.await(minerA.height)

      // 4. Restart another node with disabled mining (it has shorter chain)
      val minerB: Node = docker.startNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolume))).get

      // 5. Wait until it switches to the better chain
      Async.await(minerB.waitForHeight(minerABestHeight))

      log.info("Chain switching done")

      val minerABestBlock = Async.await(minerA.headerIdsByHeight(minerABestHeight)).head
      val minerBBestBlock = Async.await(minerB.headerIdsByHeight(minerABestHeight)).head

      minerBBestBlock shouldEqual minerABestBlock
    }

    Await.result(result, 40.minutes)

  }

}
