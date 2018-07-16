package org.ergoplatform.it

import com.typesafe.config.Config
import org.scalatest.Suite

import scala.collection.immutable.IndexedSeq

class ThreeNodesSuite extends IntegrationSuite {

  private val nodesCount = 3

  def knownPeers(nodes: Map[String, Node], nodeConfig: Config) = {
    val node01 = nodes.map(_._2).find(_.settings.scorexSettings.network.nodeName == "node01")
    val node02 = nodes.map(_._2).find(_.settings.scorexSettings.network.nodeName == "node02")

    val ls = List(node01, node02).flatten.map(_.settings.scorexSettings.network.nodeName)

    if (nodeConfig.getString("scorex.network.nodeName") == "node02" && node01.isDefined) {
      val n = node01.get
      s" -Dscorex.network.knownPeers.0=${n.nodeInfo.networkIpAddress}:${n.nodeInfo.containerNetworkPort}"
    } else
    if (nodeConfig.getString("scorex.network.nodeName") == "node03" && node02.isDefined) {
      val n = node02.get
      s" -Dscorex.network.knownPeers.0=${n.nodeInfo.networkIpAddress}:${n.nodeInfo.containerNetworkPort}"
    } else ""
  }

  protected val nodeConfigs = {
    val src = Docker.nodeConfigs.take(nodesCount)
    src.map(nonGeneratingPeerConfig.withFallback)
  }

  protected def nodeSuites(nodes: Seq[Node]): IndexedSeq[Suite] = IndexedSeq(
    new KnownNodesSpec(nodes)
  )
}
