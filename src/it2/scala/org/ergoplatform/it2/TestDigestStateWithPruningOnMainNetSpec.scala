package org.ergoplatform.it2

import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class TestDigestStateWithPruningOnMainNetSpec
  extends AnyFlatSpec
    with IntegrationSuite
    with OptionValues {

  // Persist the synced chain in a stable host dir (not the per-run random localDataDir) so
  // repeated runs resume sync instead of starting from genesis. Separate dir per state type.
  // Delete this directory to force a clean full sync.
  val localVolume = s"$tempDir/ergo-it2-mainnet-digest-pruned"
  val remoteVolume = "/app"
  Files.createDirectories(Paths.get(localVolume))

  val nodeConfig: Config = nodeSeedConfigs.head
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(digestStatePeerConfig)
    .withFallback(prunedHistoryConfig(2880))
    .withFallback(nonGeneratingPeerConfig)

  val node: Node = docker
    .startMainNetNodeYesImSure(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

  it should "Start a stateType=digest(with pruning) node on mainnet and wait for a full sync" in {
    val result = Async.async {
      Async.await(node.waitFor[NodeInfo](
        _.info,
        nodeInfo => {
          nodeInfo.bestBlockHeightOpt.exists(nodeInfo.bestHeaderHeightOpt.contains)
        },
        1.minute
      ))
    }
    Await.result(result, 4.hours)
  }

}
