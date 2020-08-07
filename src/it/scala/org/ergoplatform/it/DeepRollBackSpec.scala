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
  val chainLength = 150
  val delta = 50

  val localVolumeA = s"$localDataDir/node-rollback-spec/nodeA/data"
  val localVolumeB = s"$localDataDir/node-rollback-spec/nodeB/data"
  val remoteVolumeA = "/appA"
  val remoteVolumeB = "/appB"

  val (dirA, dirB) = (new File(localVolumeA), new File(localVolumeB))
  dirA.mkdirs(); dirB.mkdirs()

  val minerAConfig: Config = specialDataDirConfig(remoteVolumeA)
    .withFallback(shortMiningDelayConfig)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.head)

  val minerBConfig: Config = specialDataDirConfig(remoteVolumeB)
    .withFallback(shortMiningDelayConfig)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.last)

  val minerAConfigNonGen: Config = minerAConfig
    .withFallback(nonGeneratingPeerConfig)

  val minerBConfigNonGen: Config = minerBConfig
    .withFallback(nonGeneratingPeerConfig)

  "Deep rollback handling" ignore {

    val result: Future[Unit] = Async.async {

      val minerAIsolated: Node = docker.startDevNetNode(minerAConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      // 1. Let the first node mine `chainLength + delta` blocks
      Async.await(minerAIsolated.waitForHeight(chainLength + delta))

      val genesisA = Async.await(minerAIsolated.headerIdsByHeight(ErgoHistory.GenesisHeight)).head

      val minerBIsolated: Node = docker.startDevNetNode(minerBConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get

      // 2. Let another node mine `chainLength` blocks
      Async.await(minerBIsolated.waitForHeight(chainLength))

      val genesisB = Async.await(minerBIsolated.headerIdsByHeight(ErgoHistory.GenesisHeight)).head

      genesisA should not equal genesisB

      log.info("Mining phase done")

      val minerABestHeight = Async.await(minerAIsolated.fullHeight)
      val minerBBestHeight = Async.await(minerBIsolated.fullHeight)

      docker.stopNode(minerAIsolated.containerId)
      docker.stopNode(minerBIsolated.containerId)

      log.info("heightA: " + minerABestHeight)
      log.info("heightB: " + minerBBestHeight)

      (minerABestHeight > minerBBestHeight) shouldBe true

      // 3. Restart node A and node B (having shorter chain) with disabled mining
      val minerA: Node = docker.startDevNetNode(minerAConfigNonGen,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      val minerB: Node = docker.startDevNetNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get


      val isMiningAOpt = Async.await(minerA.info).isMining
      log.info("isminingA: " + isMiningAOpt)
      isMiningAOpt map (_ shouldBe false)

      val isMiningBOpt = Async.await(minerB.info).isMining
      log.info("isminingB: " + isMiningBOpt)
      isMiningBOpt map (_ shouldBe false)

      // 5. Wait until it switches to the better chain
      Async.await(minerB.waitForHeight(minerABestHeight))

      log.info("Chain switching done")

      val minerABestBlock = Async.await(minerA.headerIdsByHeight(minerABestHeight)).head
      val minerBBestBlock = Async.await(minerB.headerIdsByHeight(minerABestHeight)).head

      minerBBestBlock shouldEqual minerABestBlock
    }

    Await.result(result, 25.minutes)
  }

}
