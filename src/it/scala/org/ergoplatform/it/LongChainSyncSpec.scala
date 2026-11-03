package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class LongChainSyncSpec extends AnyFlatSpec with IntegrationSuite {

  val chainLength = 300

  // A block every 100 ms: the spec has a single miner, so no forks can form
  val minerConfig: Config = internalMinerPollingIntervalConfig(100)
    .withFallback(nodeSeedConfigs.head)
    .withFallback(allowLocalConfig)

  val nonGeneratingConfig: Config =
    nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1)).withFallback(allowLocalConfig)

  val miner: Node = docker.startDevNetNode(minerConfig).get

  it should s"Long chain ($chainLength blocks) synchronization" in {

    val result: Future[Int] = miner
      .waitForHeight(chainLength)
      .flatMap { _ =>
        val follower = docker.startDevNetNode(nonGeneratingConfig).get
        follower.waitForHeight(chainLength)
      }

    Await.result(result, 10.minutes)
  }
}
