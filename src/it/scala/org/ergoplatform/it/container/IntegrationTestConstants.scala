package org.ergoplatform.it.container

import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import Docker.ExtraConfig
import scala.collection.JavaConverters._

trait IntegrationTestConstants {

  val defaultConfigTemplate: Config = ConfigFactory.parseResources("template.conf")
  val nodesJointConfig: Config = ConfigFactory.parseResources("nodes.conf").resolve()
  val nodeSeedConfigs: List[Config] = nodesJointConfig.getConfigList("nodes").asScala.toList


  def starTopologyConfig: ExtraConfig = { (docker, nodeConfig) =>
    docker.nodes.headOption collect {
      case node if node.nodeName != nodeNameFromConfig(nodeConfig) => knownPeersConfig(Seq(node.nodeInfo))
    }
  }

  def sequentialTopologyConfig: ExtraConfig = { (docker, nodeConfig) =>
    val nodeName = nodeNameFromConfig(nodeConfig)
    val previousNode = docker.nodes.takeWhile(_.nodeName != nodeName).lastOption
    previousNode map { node => knownPeersConfig(Seq(node.nodeInfo)) }
  }

  def nodeNameFromConfig(nodeConfig: Config): String = {
    nodeConfig.as[Option[String]]("scorex.network.nodeName").getOrElse("")
  }

  def specialDataDirConfig(dir: String): Config = ConfigFactory.parseString(
    s"""
      |ergo.directory=$dir
    """.stripMargin
  )

  val nonGeneratingPeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining=false
    """.stripMargin
  )

  val onlineGeneratingPeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining=true
      |ergo.node.offlineGeneration=false
    """.stripMargin
  )

  val offlineGeneratingPeerConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.mining=true
      |ergo.node.offlineGeneration=true
    """.stripMargin
  )

  val noDelayConfig: Config = ConfigFactory.parseString(
    """
      |ergo.node.miningDelay=500ms
    """.stripMargin
  )

  def knownPeersConfig(knownNodeInfo: Seq[NodeInfo]): Config = ConfigFactory.parseString({
    val peerInfoSeq = knownNodeInfo.map(n =>
      s"""
         |"${n.networkIpAddress}:${n.containerNetworkPort}"
       """.stripMargin)
    val peerInfoStr = peerInfoSeq.mkString("[", ",", "]")
    s"""
       |scorex.network.knownPeers=$peerInfoStr
     """.stripMargin
  })

  def declaredAddressConfig(ipAddress: String, port: Int): Config = ConfigFactory.parseString(
    s"""
       |scorex.network.declaredAddress="$ipAddress:$port"
     """.stripMargin
  )

}
