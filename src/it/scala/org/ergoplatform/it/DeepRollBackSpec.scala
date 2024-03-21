package org.ergoplatform.it

import java.io.File
import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.nodeView.history.ErgoHistoryUtils
import org.scalatest.freespec.AnyFreeSpec
import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DeepRollBackSpec extends AnyFreeSpec with IntegrationSuite {

  val keepVersions = 350
  val chainLength = 50
  val delta = 150

  val localVolumeA = s"$localDataDir/node-rollback-spec/nodeA/data"
  val localVolumeB = s"$localDataDir/node-rollback-spec/nodeB/data"
  val remoteVolumeA = "/appA"
  val remoteVolumeB = "/appB"

  val (dirA, dirB) = (new File(localVolumeA), new File(localVolumeB))
  dirA.mkdirs(); dirB.mkdirs()

  val minerAConfig: Config = specialDataDirConfig(remoteVolumeA)
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.head)
    .withFallback(localOnlyConfig)

  val minerBConfig: Config = specialDataDirConfig(remoteVolumeB)
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(keepVersionsConfig(keepVersions))
    .withFallback(nodeSeedConfigs.last)
    .withFallback(localOnlyConfig)

  val minerAConfigNonGen: Config = minerAConfig
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(localOnlyConfig)

  val minerBConfigNonGen: Config = minerBConfig
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(localOnlyConfig)

  "Deep rollback handling" in {

    val result: Future[Unit] = Async.async {

      // 1. Let nodeA mine and sync nodeB

      val minerAGen: Node = docker.startDevNetNode(minerAConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      val minerBGen: Node = docker.startDevNetNode(minerBConfigNonGen,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get

      Async.await(minerAGen.waitForHeight(1))
      Async.await(minerBGen.waitForHeight(1))

      val genesisAGen = Async.await(minerAGen.headerIdsByHeight(ErgoHistoryUtils.GenesisHeight)).head
      val genesisBGen = Async.await(minerBGen.headerIdsByHeight(ErgoHistoryUtils.GenesisHeight)).head

      val minerAGenBestHeight = Async.await(minerAGen.fullHeight)
      val minerBGenBestHeight = Async.await(minerBGen.fullHeight)

      log.info("heightA: " + minerAGenBestHeight)
      log.info("heightB: " + minerBGenBestHeight)

      genesisAGen shouldBe genesisBGen

      // 2. Stop all nodes
      docker.stopNode(minerAGen.containerId)
      docker.stopNode(minerBGen.containerId)

      val minerAIsolated: Node = docker.startDevNetNode(minerAConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeA, remoteVolumeA))).get

      // 1. Let nodeA mine `chainLength + delta` blocks in isolation
      Async.await(minerAIsolated.waitForHeight(chainLength + delta))

      val minerBIsolated: Node = docker.startDevNetNode(minerBConfig, isolatedPeersConfig,
        specialVolumeOpt = Some((localVolumeB, remoteVolumeB))).get

      // 2. Let nodeB mine `chainLength` blocks in isolation
      Async.await(minerBIsolated.waitForHeight(chainLength))

      log.info("Mining phase done")

      val minerABestHeight = Async.await(minerAIsolated.fullHeight)
      val minerBBestHeight = Async.await(minerBIsolated.fullHeight)

      docker.stopNode(minerAIsolated.containerId)
      docker.stopNode(minerBIsolated.containerId)

      log.info("heightA: " + minerABestHeight)
      log.info("heightB: " + minerBBestHeight)

      (minerABestHeight > minerBBestHeight) shouldBe true

      // 3. Restart nodeA and nodeB (having shorter chain) with disabled mining
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

    Await.result(result, 15.minutes)
  }

}
