package org.ergoplatform.it

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, NetworkConfig, PortBinding}
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.asynchttpclient.Dsl._
import scorex.core.utils.ScorexLogging

import scala.collection.JavaConverters._

case class NodeInfo(
                     hostRestApiPort: Int,
                     hostNetworkPort: Int,
                     containerNetworkPort: Int,
                     apiIpAddress: String,
                     networkIpAddress: String,
                     containerId: String)

class Docker(suiteConfig: Config = ConfigFactory.empty,
             tag: String = "") extends AutoCloseable with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setRequestTimeout(10000))

  private val client = DefaultDockerClient.fromEnv().build()
  private var nodes = Map.empty[String, Node]
  private var seedAddress = Option.empty[String]
  private val isStopped = new AtomicBoolean(false)

  sys.addShutdownHook {
    close()
  }

  private def knownPeers = seedAddress.fold("")(sa => s"-Dscorex.network.knownPeers.0=$sa")

  private val networkName = "inner-" + this.##.toLong.toHexString

  private val innerNetwork = client.createNetwork(NetworkConfig.builder().driver("bridge").name(networkName).build())

  def startNode(nodeConfig: Config): Node = {
    val configOverrides = s"$knownPeers ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))}"
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(DefaultConfigTemplate)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()

    val restApiPort = actualConfig.getString("scorex.restApi.bindAddress").split(":")(1)
    val networkPort = actualConfig.getString("scorex.network.bindAddress").split(":")(1)

    val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
      .put(restApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(networkPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .build()

    val hostConfig = HostConfig.builder()
      .portBindings(portBindings)
      .build()

    val containerConfig = ContainerConfig.builder()
      .image("org.ergoplatform/ergo:latest")
      .exposedPorts(restApiPort, networkPort)
      .hostConfig(hostConfig)
      .env(s"OPTS=$configOverrides", s"PORT=$networkPort")
      .build()

    val containerId = client.createContainer(containerConfig, actualConfig.getString("scorex.network.nodeName") + "-" +
      this.##.toLong.toHexString).id()
    connectToNetwork(containerId)
    client.startContainer(containerId)
    val containerInfo = client.inspectContainer(containerId)

    val ports = containerInfo.networkSettings().ports()

    val nodeInfo = NodeInfo(
      extractHostPort(ports, restApiPort),
      extractHostPort(ports, networkPort),
      networkPort.toInt,
      containerInfo.networkSettings().ipAddress(),
      containerInfo.networkSettings().networks().asScala(networkName).ipAddress(),
      containerId)
    val node = new Node(actualConfig, nodeInfo, http)
    if (seedAddress.isEmpty) {
      seedAddress = Some(s"${nodeInfo.networkIpAddress}:${nodeInfo.containerNetworkPort}")
    }
    nodes += containerId -> node
    node
  }

  def stopNode(containerId: String): Unit = {
    client.stopContainer(containerId, 10)
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodes.foreach {
        case (id, n) =>
          n.close()
          client.stopContainer(id, 0)
      }
      http.close()

      saveLogs()

      nodes.keys.foreach(id => client.removeContainer(id, RemoveContainerParam.forceKill()))
      client.removeNetwork(innerNetwork.id())
      client.close()
    }
  }

  private def saveLogs(): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    nodes.values.foreach { node =>
      import node.nodeInfo.containerId

      val fileName = if (tag.isEmpty) containerId else s"$tag-$containerId"
      val logFile = logDir.resolve(s"$fileName.log").toFile
      log.info(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

      val fileStream = new FileOutputStream(logFile, false)
      client
        .logs(
          containerId,
          DockerClient.LogsParam.timestamps(),
          DockerClient.LogsParam.follow(),
          DockerClient.LogsParam.stdout(),
          DockerClient.LogsParam.stderr()
        )
        .attach(fileStream, fileStream)
    }
  }

  def disconnectFromNetwork(containerId: String): Unit = client.disconnectFromNetwork(containerId, innerNetwork.id())

  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)

  def connectToNetwork(containerId: String): Unit = client.connectToNetwork(containerId, innerNetwork.id())

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId)
}

object Docker {
  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def apply(owner: Class[_]): Docker = new Docker(tag = owner.getSimpleName)

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt

  val DefaultConfigTemplate = ConfigFactory.parseResources("template.conf")
  val NodeConfigs = ConfigFactory.parseResources("nodes.conf")
}
