package org.ergoplatform.it

import org.scalatest.FreeSpec

import scala.async.Async.{async, await}
import scala.concurrent.Await
import scala.concurrent.duration._

class LongChainSpec extends FreeSpec with IntegrationSuite {

    s"Synchronize long blocks" ignore {
    val minerConfig = noDelayConfig.withFallback(Docker.nodeConfigs.head)
    val followerConfig = nonGeneratingPeerConfig.withFallback(Docker.nodeConfigs(1))
    val miner = docker.startNode(minerConfig).success.value
    val blocksCount = 10
    val check = async {
      await(miner.waitForHeight(blocksCount))
      val follower = docker.startNode(followerConfig).success.value
      await(follower.waitForHeight(blocksCount))
      succeed
    }
    Await.result(check, 3.minutes)
  }
}

