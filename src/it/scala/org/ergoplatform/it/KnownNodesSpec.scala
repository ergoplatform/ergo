package org.ergoplatform.it

import com.typesafe.config.Config
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class KnownNodesSpec extends FreeSpec with IntegrationSuite {

  override val docker: Docker  = Docker.sequentialTopology(getClass)

  val nodeConfigs = Docker.nodeConfigs.take(3).map(nonGeneratingPeerConfig.withFallback)
  val nodes: List[Node] = docker.startNodes(nodeConfigs).success.value

  s"The third node knows first node" in {

    val node01 = nodes.find(_.settings.scorexSettings.network.nodeName == "node01")
    val node03 = nodes.find(_.settings.scorexSettings.network.nodeName == "node03")

    val result = node03.map(_.waitForPeers(2).map { peers =>
      peers.map(_.name) should contain("node01")
    }).get

    Await.result(result, 10.minutes)
  }

}

