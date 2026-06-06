package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * End-to-end reproduction of a pruning node that fell behind by more than its `blocksToKeep` window
  * while it was offline. After such a shutdown the network tip - and therefore `minimalFullBlockHeight`
  * after the headers re-sync - is far above the node's local best full block, and the node has to apply
  * the intermediate full blocks to catch up. A node that refuses those intermediate blocks as "too old"
  * gets permanently stuck at its pre-shutdown height; a healthy node downloads them and reaches the tip.
  */
class PrunedDigestNodeCatchUpSpec
  extends AnyFlatSpec
    with IntegrationSuite
    with OptionValues {

  private val blocksToKeep = 5
  // height the pruned node reaches before going offline (best full block inside the keep window)
  private val syncHeight = 8

  private val remoteVolume = "/app"
  private val minerVolume = s"$localDataDir/pruned-catchup-spec/miner"
  private val digestVolume = s"$localDataDir/pruned-catchup-spec/digest"
  // start each run from clean volumes so a previous run's chain/state cannot interfere with recovery
  freshDir(minerVolume)
  freshDir(digestVolume)

  // single miner that keeps mining throughout, so the chain advances while the pruned node is offline
  private val minerConfig: Config = offlineGeneratingPeerConfig
    .withFallback(shortInternalMinerPollingInterval)
    .withFallback(blockIntervalConfig(500))
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(nodeSeedConfigs.head)
    .withFallback(localOnlyConfig)

  // pruning light (digest) node that syncs, goes offline, then has to catch up
  private val digestConfig: Config = digestStatePeerConfig
    .withFallback(prunedHistoryConfig(blocksToKeep))
    .withFallback(blockIntervalConfig(500))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(nodeSeedConfigs(1))
    .withFallback(localOnlyConfig)

  // Testing scenario:
  // 1. Start the miner and a pruning digest node; let the digest node sync ({stoppedAt} full blocks);
  // 2. Stop the digest node gracefully (the "long shutdown") and let the miner mine past
  //    {stoppedAt + blocksToKeep}, so every block the node still needs is below minimalFullBlockHeight;
  // 3. Restart the digest node and assert it catches up to {target} instead of being stuck at {stoppedAt}.
  it should "Pruned digest node catches up after being offline longer than blocksToKeep" in {

    val minerNode: Node =
      docker.startDevNetNode(minerConfig, specialVolumeOpt = Some((minerVolume, remoteVolume))).get
    val digestNode: Node =
      docker.startDevNetNode(digestConfig, specialVolumeOpt = Some((digestVolume, remoteVolume))).get

    val result = for {
      _         <- digestNode.waitForHeight(syncHeight)
      // best full block height of the pruning node at the moment it goes offline
      stoppedAt <- digestNode.fullHeight
      // graceful stop so DigestState is flushed and can be reloaded on restart
      _          = docker.stopNode(digestNode, secondsToWait = 20)
      // the chain must advance strictly more than blocksToKeep beyond where the node stopped
      target     = stoppedAt + blocksToKeep + 6
      _         <- minerNode.waitForHeight(target)
      restarted  = docker
        .startDevNetNode(digestConfig, specialVolumeOpt = Some((digestVolume, remoteVolume))).get
      // with the fix the node downloads the now-"too old" intermediate blocks and crosses the window;
      // without it the node stays stuck at stoppedAt and this wait times out
      caughtUp  <- restarted.waitForHeight(target)
    } yield caughtUp should be >= target

    Await.result(result, 10.minutes)
  }

  private def freshDir(path: String): Unit = {
    val dir = new File(path)
    def delete(f: File): Unit = {
      if (f.isDirectory) Option(f.listFiles()).foreach(_.foreach(delete))
      f.delete()
    }
    delete(dir)
    dir.mkdirs()
  }

}
