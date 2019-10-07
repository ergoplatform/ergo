package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class KnownNodesSpec extends FreeSpec with IntegrationSuite {

  val nodeConfigs: List[Config] = nodeSeedConfigs.take(3).map(nonGeneratingPeerConfig.withFallback)
  val nodes: List[Node] = docker.startTestNetNodes(nodeConfigs, sequentialTopologyConfig).get

  // todo: https://github.com/ergoplatform/ergo/issues/653
  s"The third node knows first node" ignore {

    val node03 = nodes.find(_.nodeName == "node03").value
    val targetPeersCount = nodes.length - 1 /* self */
    val result = node03.waitForPeers(targetPeersCount).map { peers =>
      peers.map(_.name) should contain("node01")
    }

    Await.result(result, 10.minute)
  }

}
