package org.ergoplatform.it

import com.typesafe.config.Config
import org.scalatest.Suite

import scala.collection.immutable.IndexedSeq

class FourNodesSuite extends IntegrationSuite {

  private val nodesCount = 4

  def knownPeers(nodes: Map[String, Node], nodeConfig: Config) = {
    val node01 = nodes.map(_._2).find(_.settings.scorexSettings.network.nodeName == "node01")
    node01 match {
      case Some(n) if n != nodeConfig.getString("scorex.network.nodeName")=>
        s" -Dscorex.network.knownPeers.0=${n.nodeInfo.networkIpAddress}:${n.nodeInfo.containerNetworkPort}"
      case None => ""
    }
  }

  protected val nodeConfigs = {
    val src = Docker.nodeConfigs.take(nodesCount)
    src.head +:
      nonGeneratingPeerConfig.withFallback(src.tail.head) +:
      src.tail.tail.map(onlineGeneratingPeerConfig.withFallback)
  }

  protected def nodeSuites(nodes: Seq[Node]): IndexedSeq[Suite] = IndexedSeq(
    new NodesSynchronizationSpec(nodes)
  )

}
