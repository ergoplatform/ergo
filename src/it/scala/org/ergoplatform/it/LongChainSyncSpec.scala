package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class LongChainSyncSpec extends AnyFlatSpec with IntegrationSuite {

  val chainLength = 300

  val minerConfig: Config = shortInternalMinerPollingInterval
    .withFallback(nodeSeedConfigs.head)
    .withFallback(localOnlyConfig)

  val nonGeneratingConfig: Config =
    nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1)).withFallback(localOnlyConfig)

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
