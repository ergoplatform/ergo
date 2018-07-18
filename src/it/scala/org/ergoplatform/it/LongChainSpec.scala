package org.ergoplatform.it

import com.typesafe.config.Config
import org.scalatest.FreeSpec

import scala.async.Async.{async, await}
import scala.concurrent.Await
import scala.concurrent.duration._

class LongChainSpec extends FreeSpec with IntegrationSuite {

  s"Synchronize long blocks" in {
    val minerConfig = noDelayConfig.withFallback(Docker.nodeConfigs.head)
    val followerConfig = nonGeneratingPeerConfig.withFallback(Docker.nodeConfigs(1))
    val miner = docker.startNode(minerConfig).success.value
    val blocksCount = 300
      val check = async {
        await(miner.waitForHeight(blocksCount))
        val follower = docker.startNode(followerConfig).success.value
        await(follower.waitForHeight(blocksCount))
      succeed
    }
      Await.result(check, 15.minutes)
  }
}

