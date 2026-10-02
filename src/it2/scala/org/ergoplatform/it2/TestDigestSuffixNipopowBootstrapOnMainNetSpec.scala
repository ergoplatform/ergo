package org.ergoplatform.it2

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Try
import io.circe.parser.parse

class TestDigestSuffixNipopowBootstrapOnMainNetSpec
  extends AnyFlatSpec
    with IntegrationSuite
    with OptionValues {

  // Configuration under test: NiPoPoW bootstrap + stateType = digest + a full-block suffix
  // (blocksToKeep). No UTXO snapshot is involved, so there is no snapshot phase: after the
  // NiPoPoW proof is applied and headers are synced, the node must start downloading and
  // applying full blocks on its own.
  //
  // The node bootstraps from the NiPoPoW proof its peers serve (the stored proof taken 11 blocks
  // before the last UTXO-snapshot epoch boundary), syncs the remaining headers normally, and must
  // then apply the full-block suffix. This passes on mainnet once the header at which header sync
  // is declared synced (the first header younger than headerChainDiff x blockInterval, ~85-100
  // blocks below the tip) is at least blocksToKeep - 1 blocks past the snapshot boundary, i.e. a
  // tip roughly blocksToKeep + 100 blocks past that boundary. Inside that window after each snapshot
  // boundary (~4.2 days every ~72.5 days for blocksToKeep = 2880) the pruning floor falls into the
  // proof's sparse prefix (ergoplatform/ergo#2595) and no full block is applied; the
  // "first full block applied" await below reports that. The in-window stall is expected from
  // the code and has not been observed on mainnet.
  //
  // The node data dir is mounted from a host temp directory (not an anonymous container volume)
  // so that the container can be killed and restarted with the same data directory.
  // A fresh (empty) host directory is created per run, so every run still performs a real
  // bootstrap from scratch.

  val bootstrapConfig: Config = ConfigFactory.parseString(
    s"""
       |ergo.node.utxo.utxoBootstrap = false
       |ergo.node.nipopow.nipopowBootstrap = true
       |# genesisId of mainnet.conf, needed for ErgoSettings validation on the test host,
       |# where the network config file is not loaded (see Docker.buildErgoSettings)
       |ergo.chain.genesisId = "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b"
    """.stripMargin
  )

  // ~4 days of full blocks, same value as TestDigestStateWithPruningOnMainNetSpec.
  // Do not add ergo.node.extraIndex here: it is rejected together with pruning (ErgoSettingsReader).
  val blocksToKeep = 2880

  val localVolume: String = s"$localDataDir/test-digest-suffix-nipopow-bootstrap/data"
  val remoteVolume: String = "/home/ergo/.ergo"
  new File(localVolume).mkdirs()

  val nodeConfig: Config = bootstrapConfig
    .withFallback(digestStatePeerConfig)
    .withFallback(prunedHistoryConfig(blocksToKeep))
    .withFallback(specialDataDirConfig(remoteVolume))
    .withFallback(nodeSeedConfigs.head)
    .withFallback(nonGeneratingPeerConfig)

  val node: Node = docker
    .startMainNetNodeYesImSure(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume)))
    .get

  // makeSnapshotEvery on mainnet
  private val SnapshotEvery = 52224
  // blocksToKeep plus ~100: header sync is declared synced about 85-100 blocks below the tip
  private val WindowLength = blocksToKeep + 100
  // maxPeerHeight is 0 until peers have reported their heights; mainnet is far above this
  private val MinPlausibleTip = 1000000

  // Peers' best height (maxPeerHeight in /info): the chain tip as the node sees it
  private def peersTip(n: Node): Option[Int] =
    Try(Await.result(n.status, 30.seconds)).toOption
      .flatMap(s => parse(s.status).toOption)
      .flatMap(_.hcursor.downField("maxPeerHeight").as[Option[Int]].toOption.flatten)
      .filter(_ > MinPlausibleTip)

  // The node's current header and full heights, for timeout messages
  private def heightsOf(n: Node): String =
    Try(Await.result(n.info, 30.seconds)).toOption
      .fold("node info unavailable")(i => s"header height ${i.bestHeaderHeightOpt}, full height ${i.bestBlockHeightOpt}")

  // Says whether the run started inside the post-snapshot window (#2595), from the peers' tip sampled just before the
  // kill (or, failing that, at failure time), not from the node's own header height, which lags the tip during header sync.
  private def windowHint(n: Node, tipAtStart: Option[Int]): String = {
    val tip = tipAtStart.map(t => ("at start", t)).orElse(peersTip(n).map(t => ("at failure", t)))
    val where = tip.fold("peers' tip unknown") { case (when, t) =>
      val r = (t + 11) % SnapshotEvery
      val verdict = if (r < WindowLength) {
        "inside the post-snapshot window, ergoplatform/ergo#2595"
      } else {
        "outside the post-snapshot window, not ergoplatform/ergo#2595"
      }
      s"peers' tip $when $t, (t + 11) % $SnapshotEvery = $r: $verdict"
    }
    s"${heightsOf(n)}; $where"
  }

  // Waits for the future, converting a timeout into a test failure with a named message.
  // The failure text is evaluated only on timeout.
  private def awaitNamed[T](what: String, timeout: FiniteDuration, failureText: => String)(f: => Future[T]): T =
    try {
      Await.result(f, timeout)
    } catch {
      case _: java.util.concurrent.TimeoutException =>
        fail(s"[$what] $failureText")
    }

  it should "Bootstrap a pruned digest node via NiPoPoW proof on mainnet, survive a restart and sync the full-block suffix" in {
    // Phase 1: headers appear, proving the trusted NiPoPoW proof was applied
    val nodeInfoAfterHeaders = awaitNamed("headers appear", 1.hour,
      "no headers beyond height 1000 within 1 hour after start") {
      Async.async {
        Async.await(node.waitFor[NodeInfo](
          _.info,
          nodeInfo => nodeInfo.bestHeaderHeightOpt.exists(_ > 1000),
          1.minute
        ))
      }
    }
    log.info(s"Headers appeared, best header height: ${nodeInfoAfterHeaders.bestHeaderHeightOpt}")

    // A NiPoPoW-bootstrapped header chain is sparse below the proof suffix: an ordinary header
    // sync from genesis would have a header at height 2, a NiPoPoW bootstrap does not.
    Await.result(node.headerIdsByHeight(2), 1.minute) shouldBe empty

    // Phase 2: kill the node after headers appeared. Depending on timing this is before the
    // first full block or after full blocks started applying; either way the restart exercises
    // recovery of a NiPoPoW-bootstrapped pruned digest node from its on-disk state (a digest node
    // has no snapshot phase). The wait for a first full block is bounded to 90 seconds.
    val killWindowStart = System.currentTimeMillis()
    val preKillInfo = Await.result(Async.async {
      var info = nodeInfoAfterHeaders
      while (info.bestBlockHeightOpt.isEmpty &&
             System.currentTimeMillis() - killWindowStart < 90.seconds.toMillis) {
        Thread.sleep(5.seconds.toMillis)
        info = Async.await(node.waitFor[NodeInfo](_.info, _ => true, 1.minute))
      }
      info
    }, 5.minutes)
    log.info(s"Killing node, best header height: ${preKillInfo.bestHeaderHeightOpt}, " +
      s"best full block height: ${preKillInfo.bestBlockHeightOpt}")
    // The peers' tip is not known right after the proof is applied (maxPeerHeight is still 0); by the kill,
    // 5-95 s later, header sync from peers is under way. This is the chain tip the run started at.
    val tipAtStart = peersTip(node)
    log.info(s"Peers' tip before the kill: $tipAtStart")
    docker.forceStopNode(node.containerId)

    // Phase 3: restart with the same data directory
    val restartedNode = docker
      .startMainNetNodeYesImSure(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume)))
      .get

    // Phase 3b: the restarted node's API must come up and report at least the pre-kill header height
    // and, when full blocks had been applied before the kill, at least the pre-kill full height.
    // Named separately so a failed recovery from the on-disk state is not reported as a phase-4
    // timeout and mistaken for #2595.
    val preKillHeaderHeight = preKillInfo.bestHeaderHeightOpt.orElse(nodeInfoAfterHeaders.bestHeaderHeightOpt).value
    awaitNamed("node recovered after restart", 10.minutes,
      s"restarted node did not report the pre-kill header height $preKillHeaderHeight " +
        s"(full height ${preKillInfo.bestBlockHeightOpt}) within 10 minutes; " +
        "recovery from the on-disk state failed (this is not ergoplatform/ergo#2595); " +
        "restarted node now reports: " + heightsOf(restartedNode)) {
      Async.async {
        Async.await(restartedNode.waitFor[NodeInfo](
          _.info,
          nodeInfo => nodeInfo.bestHeaderHeightOpt.exists(_ >= preKillHeaderHeight) &&
            preKillInfo.bestBlockHeightOpt.forall(f => nodeInfo.bestBlockHeightOpt.exists(_ >= f)),
          10.seconds
        ))
      }
    }
    log.info(s"Restarted node recovered, pre-kill header height was $preKillHeaderHeight, " +
      s"pre-kill full height was ${preKillInfo.bestBlockHeightOpt}")

    // Phase 4: the first full block must be applied. Height 1 does not count: on #2595's devnet the
    // floor latched at 1 and fullHeight stayed 1. On mainnet a stalled node's floor is a voting-epoch
    // start below the snapshot boundary (never 1), so by reading fullHeight stays empty; either way
    // this await times out.
    // Outside the post-snapshot window described at the top, the first full block follows header
    // sync within minutes; 2 hours is a generous bound. A timeout here most likely means the run
    // started inside that window (#2595).
    val firstFullBlockInfo = awaitNamed("first full block applied", 2.hours,
      "no full block applied within 2 hours after NiPoPoW header sync; " + windowHint(restartedNode, tipAtStart)) {
      Async.async {
        Async.await(restartedNode.waitFor[NodeInfo](
          _.info,
          nodeInfo => nodeInfo.bestBlockHeightOpt.exists(_ > 1),
          1.minute
        ))
      }
    }
    log.info(s"First full block applied, full height: ${firstFullBlockInfo.bestBlockHeightOpt}, " +
      s"best header height: ${firstFullBlockInfo.bestHeaderHeightOpt}")

    // Phase 5: the full-block suffix catches up with the header chain. Observed: minutes. Capped at
    // 1 hour so that the awaits sum to well under the 6 h job limit and a stall ends as this named
    // failure, not as a job cancellation.
    val syncedInfo = awaitNamed("suffix synced", 1.hour,
      "full height did not reach header height within 1 hour after the first full block; " + heightsOf(restartedNode)) {
      Async.async {
        Async.await(restartedNode.waitFor[NodeInfo](
          _.info,
          nodeInfo => nodeInfo.bestBlockHeightOpt.exists(nodeInfo.bestHeaderHeightOpt.contains),
          1.minute
        ))
      }
    }

    // Pruning invariant: at most blocksToKeep full blocks behind the header tip. Trivially true
    // once full height == header height; kept as the documented invariant.
    syncedInfo.bestBlockHeightOpt.value should be >= (syncedInfo.bestHeaderHeightOpt.value - blocksToKeep)
    // guard against a degenerate "synced at genesis" pass
    syncedInfo.bestHeaderHeightOpt.value should be > 1000000
  }

}
