package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.api.NodeApi.Peer
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class KnownNodesSpec extends AnyFlatSpec with IntegrationSuite {

  val nodeConfigs: List[Config] = nodeSeedConfigs.take(3)
    .map(conf => nonGeneratingPeerConfig
      .withFallback(conf)
      .withFallback(allowLocalConfig)
    )

  val nodes: List[Node] = docker.startDevNetNodes(nodeConfigs, sequentialTopologyConfig).get

  // All nodes should propagate sequentially, so any node knows each other
  it should s"The third node knows first node" in {

    val node03 = nodes.find(_.nodeName == "node03").value
    val expectedNames = nodes.filterNot(_ == node03).map(_.nodeName).toSet
    // Reciprocal transports can name the same node; wait for the expected nodes themselves.
    val result = node03.waitFor[Seq[Peer]](_.connectedPeers,
      peers => expectedNames.subsetOf(peers.map(_.name).toSet), 1.second).map { peers =>
      peers.map(_.name).toSet should contain allElementsOf expectedNames
    }

    Await.result(result, 10.minute)
  }

}
