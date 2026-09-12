package org.ergoplatform.it2

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

  // Unlike TestOnMainNetSpec, no host volume is mounted: node data lives in the container's
  // anonymous volume (/home/ergo/.ergo) and is discarded with the container, so every run
  // performs a real bootstrap from an empty data dir.

  val bootstrapConfig: Config = ConfigFactory.parseString(
    s"""
       |ergo.node.utxo.utxoBootstrap = true
       |ergo.node.nipopow.nipopowBootstrap = true
       |# genesisId of mainnet.conf, needed for ErgoSettings validation on the test host,
       |# where the network config file is not loaded (see Docker.buildErgoSettings)
       |ergo.chain.genesisId = "b0244dfc267baca974a4caee06120321562784303a8a688976ae56170e4d175b"
    """.stripMargin
  )

  val nodeConfig: Config = bootstrapConfig
    .withFallback(nodeSeedConfigs.head)
    .withFallback(nonGeneratingPeerConfig)
  val node: Node = docker.startMainNetNodeYesImSure(nodeConfig).get

  it should "Bootstrap from a UTXO set snapshot via NiPoPoW proof on mainnet and fully sync" in {
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

    // Phase 2: wait for a full sync (snapshot applied + full blocks downloaded)
    val syncResult = Async.async {
      Async.await(node.waitFor[NodeInfo](
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
