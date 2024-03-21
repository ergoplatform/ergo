package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class KnownNodesSpec extends AnyFlatSpec with IntegrationSuite {

  val nodeConfigs: List[Config] = nodeSeedConfigs.take(3)
    .map(conf => nonGeneratingPeerConfig
      .withFallback(conf)
      .withFallback(localOnlyConfig)
    )

  val nodes: List[Node] = docker.startDevNetNodes(nodeConfigs, sequentialTopologyConfig).get

  // All nodes should propagate sequentially, so any node knows each other
  it should s"The third node knows first node" in {

    val node03 = nodes.find(_.nodeName == "node03").value
    val targetPeersCount = nodes.length - 1 /* self */
    val result = node03.waitForPeers(targetPeersCount).map { peers =>
      peers.map(_.name) should contain("node01")
    }

    Await.result(result, 10.minute)
  }

}
