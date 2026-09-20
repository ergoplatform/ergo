package org.ergoplatform.it2

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class TestUtxoSnapshotBootstrapOnMainNetSpec
  extends AnyFlatSpec
    with IntegrationSuite
    with OptionValues {

  // The node data dir is mounted from a host temp directory (not an anonymous container volume)
  // so that the container can be killed and restarted with the same data directory, which is
  // needed to exercise resuming UTXO set snapshot bootstrapping after a restart.
  // A fresh (empty) host directory is created per run, so every run still performs a real
  // bootstrap from scratch.

  val bootstrapConfig: Config = ConfigFactory.parseString(
    s"""
       |ergo.node.utxo.utxoBootstrap = true
       |ergo.node.nipopow.nipopowBootstrap = true
       |# genesisId of mainnet.conf, needed for ErgoSettings validation on the test host,
       |# where the network config file is not loaded (see Docker.buildErgoSettings)
       |ergo.chain.genesisId = "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b"
    """.stripMargin
  )

  val localVolume: String = s"$localDataDir/test-utxo-snapshot-bootstrap/data"
  val remoteVolume: String = "/home/ergo/.ergo"
  new File(localVolume).mkdirs()

  val nodeConfig: Config = bootstrapConfig
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(nodeSeedConfigs.head)
    .withFallback(nonGeneratingPeerConfig)

  val node: Node = docker
    .startMainNetNodeYesImSure(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume)))
    .get

  it should "Bootstrap from a UTXO set snapshot via NiPoPoW proof on mainnet, survive a restart during snapshot download and fully sync" in {
    // Phase 1: headers appear, proving the trusted NiPoPoW proof was applied
    val headersResult = Async.async {
      Async.await(node.waitFor[NodeInfo](
        _.info,
        nodeInfo => nodeInfo.bestHeaderHeightOpt.exists(_ > 1000),
        1.minute
      ))
    }
    val nodeInfoAfterHeaders = Await.result(headersResult, 1.hour)
    log.info(s"Headers appeared, best header height: ${nodeInfoAfterHeaders.bestHeaderHeightOpt}")

    // Phase 2: wait until the node is in the middle of UTXO set snapshot downloading, then kill it.
    // No full blocks can exist before the snapshot is applied, so bestBlockHeightOpt stays empty
    // during the whole snapshot phase; on mainnet chunk download alone takes minutes, so killing
    // 90+ seconds after the headers appeared usually lands the restart in the middle of snapshot
    // bootstrapping. If the snapshot finished faster than the window, the restart still exercises
    // startup recovery - from a post-snapshot state - so we branch instead of failing the run.
    val snapshotPhaseStart = System.currentTimeMillis()
    val preKillInfo = Await.result(Async.async {
      var info = nodeInfoAfterHeaders
      while (info.bestBlockHeightOpt.isEmpty &&
             System.currentTimeMillis() - snapshotPhaseStart < 90.seconds.toMillis) {
        Thread.sleep(5.seconds.toMillis)
        info = Async.await(node.waitFor[NodeInfo](_.info, _ => true, 1.minute))
      }
      info
    }, 5.minutes)
    if (preKillInfo.bestBlockHeightOpt.isEmpty) {
      log.info(s"Killing node mid snapshot bootstrap, best header height: ${preKillInfo.bestHeaderHeightOpt}")
    } else {
      log.info("Snapshot applied before the kill window elapsed; restarting from a post-snapshot state instead")
    }
    docker.forceStopNode(node.containerId)

    // Phase 3: restart with the same data directory and require the snapshot bootstrap to
    // complete afterwards (snapshot applied, then full blocks downloaded to the tip)
    val restartedNode = docker
      .startMainNetNodeYesImSure(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume)))
      .get

    val syncResult = Async.async {
      Async.await(restartedNode.waitFor[NodeInfo](
        _.info,
        nodeInfo => nodeInfo.bestBlockHeightOpt.exists(nodeInfo.bestHeaderHeightOpt.contains),
        1.minute
      ))
    }
    val syncedInfo = Await.result(syncResult, 5.hours)

    // guard against a degenerate "synced at genesis" pass
    syncedInfo.bestHeaderHeightOpt.value should be > 1000000
  }

}
