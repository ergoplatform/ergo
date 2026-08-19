package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UtxoStateNodesSyncSpec extends AnyFlatSpec with IntegrationSuite {

  val blocksQty = 5

  val forkDepth: Int      = blocksQty
  val minerConfig: Config = nodeSeedConfigs.head

  val nonGeneratingConfig: Config =
    nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))

  val onlineGeneratingConfigs: List[Config] =
    nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)

  val nodeConfigs: List[Config] =
    (minerConfig +: nonGeneratingConfig +: onlineGeneratingConfigs)
      .map(_.withFallback(allowLocalConfig))

  val nodes: List[Node] = docker.startDevNetNodes(nodeConfigs).get

  it should s"Utxo state nodes synchronisation ($blocksQty blocks)" in {
    val result = for {
      initHeight <- Future.traverse(nodes)(_.fullHeight).map(x => math.max(x.max, 1))
      _          <- Future.traverse(nodes)(_.waitForHeight(initHeight + blocksQty))
      headers <- Future.traverse(nodes)(
                  _.headerIdsByHeight(initHeight + blocksQty - forkDepth)
                )
    } yield {
      log.info(
        s"Headers at height ${initHeight + blocksQty - forkDepth}: ${headers.mkString(",")}"
      )
      // `/blocks/at/{height}` returns *every* header id known at the given height, with the
      // best-chain one first (see `HeadersProcessor.headerIdsAtHeight`). Nodes are in sync
      // when their best-chain header at that height matches; a node may legitimately also
      // know orphaned headers of a fork that was already resolved, so flattening all the
      // returned ids makes the assertion fail on a perfectly synchronised network.
      // Same convention as ForkResolutionSpec and DeepRollBackSpec, which compare `.head`.
      headers.foreach(_ should not be empty)
      val bestChainHeaderIds = headers.map(_.head)
      val sample             = bestChainHeaderIds.head
      bestChainHeaderIds should contain only sample
    }
    Await.result(result, 15.minutes)
  }

}
