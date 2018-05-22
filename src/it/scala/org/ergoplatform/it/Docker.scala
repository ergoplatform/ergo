package org.ergoplatform.it

import java.io.FileOutputStream
import java.net.InetAddress
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, UUID, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.google.common.primitives.Ints
import com.spotify.docker.client.DockerClient._
import com.spotify.docker.client.exceptions.ImageNotFoundException
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import net.ceedubs.ficus.Ficus._
import org.asynchttpclient.Dsl._
import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.ScorexLogging

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Try}

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

  // This should be called after client is ready but before network created.
  // This allows resource cleanup for the network if we are running out of them
  initBeforeStart()

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000
  private val networkPrefix = s"${InetAddress.getByAddress(Ints.toByteArray(networkSeed)).getHostAddress}/28"
  private val innerNetwork: Network = createNetwork(3)

  def initBeforeStart(): Unit = {
    cleanupDanglingIfNeeded()
    sys.addShutdownHook {
      close()
    }
  }

  def startNodes(nodeConfigs: Seq[Config]): Future[Seq[Node]] = {
    log.trace(s"Starting ${nodeConfigs.size} containers")
    val tryNodes: Seq[Try[Node]] = nodeConfigs.map(startNode)
    log.debug("Waiting for nodes to start")
    blocking(Thread.sleep(tryNodes.size * 5000))
    val futureNodes: Seq[Future[Node]] = tryNodes map { tryNode =>
      Future.fromTry(tryNode.map(node => node.waitForStartup)).flatten
    }
    Future.sequence(futureNodes)
  }

  def startNode(nodeConfig: Config): Try[Node] = {
    val settings: ErgoSettings = buildErgoSettings(nodeConfig)

    val configuredNodeName = settings.scorexSettings.network.nodeName
    val nodeNumber = configuredNodeName.replace("node", "").toInt
    val ip = ipForNode(nodeNumber)

    val restApiPort = settings.scorexSettings.restApi.bindAddress.getPort
    val networkPort = settings.scorexSettings.network.bindAddress.getPort

    val containerConfig: ContainerConfig = buildContainerConfig(nodeConfig, ip, restApiPort, networkPort)
    val containerName = networkName + "-" + configuredNodeName + "-" + uuidShort

    Try {
      val containerId = client.createContainer(containerConfig, containerName).id
      val attachedNetwork = connectToNetwork(containerId, ip)
      client.startContainer(containerId)

      val containerInfo = client.inspectContainer(containerId)
      val ports = containerInfo.networkSettings().ports()

      val nodeInfo = NodeInfo(
        hostRestApiPort = extractHostPort(ports, restApiPort),
        hostNetworkPort = extractHostPort(ports, networkPort),
        containerNetworkPort = networkPort,
        apiIpAddress = containerInfo.networkSettings().ipAddress(),
        networkIpAddress = attachedNetwork.ipAddress(),
        containerId = containerId)

      log.info(s"Started node: $nodeInfo")
      val node = new Node(settings, nodeInfo, http)

      if (seedAddress.isEmpty) {
        seedAddress = Some(s"${nodeInfo.networkIpAddress}:${nodeInfo.containerNetworkPort}")
      }
      nodes += containerId -> node
      node
    } recoverWith {
      case e: ImageNotFoundException =>
        Failure(new Exception(s"Error: docker image is missing. Run 'sbt it:test' to generate it.", e))
    }
  }

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(props: Properties): String =
    props.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(portBindingMap: JMap[String, JList[PortBinding]], containerPort: Int): Int =
    portBindingMap.get(s"$containerPort/tcp").get(0).hostPort().toInt

  private def buildErgoSettings(nodeConfig: Config) = {
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(defaultConfigTemplate)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
    val settings = ErgoSettings.fromConfig(actualConfig)
    settings
  }

  private def buildContainerConfig(nodeConfig: Config, ip: String, restApiPort: Int, networkPort: Int) = {

    val portBindings = new ImmutableMap.Builder[String, JList[PortBinding]]()
      .put(restApiPort.toString, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(networkPort.toString, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .build()

    val hostConfig = HostConfig.builder()
      .portBindings(portBindings)
      .memory(1L << 30) //limit memory to 1G
      .build()

    val networkingConfig = ContainerConfig.NetworkingConfig
      .create(Map(networkName -> endpointConfigFor(ip)).asJava)

    val knownPeersSetting = seedAddress.fold("")(sa => s" -Dscorex.network.knownPeers.0=$sa")

    val configOverrides = renderProperties(asProperties(nodeConfig.withFallback(suiteConfig))) +
                          knownPeersSetting

    ContainerConfig.builder()
      .image("org.ergoplatform/ergo:latest")
      .exposedPorts(restApiPort.toString, networkPort.toString)
      .networkingConfig(networkingConfig)
      .hostConfig(hostConfig)
      .env(s"OPTS=$configOverrides")
      .build()
  }

  private def createNetwork(maxRetry: Int = 5): Network = try {
    val params = DockerClient.ListNetworksParam.byNetworkName(networkName)
    val networkOpt = client.listNetworks(params).asScala.headOption
    networkOpt match {
      case Some(network) =>
        log.info(s"Network ${network.name()} (id: ${network.id()}) is created for $tag, " +
          s"ipam: ${ipamToString(network)}")
        network
      case None =>
        log.debug(s"Creating network $networkName for $tag")
        // Specify the network manually because of race conditions: https://github.com/moby/moby/issues/20648
        val r = client.createNetwork(buildNetworkConfig())
        Option(r.warnings()).foreach(log.warn(_))
        createNetwork(maxRetry - 1)
    }
  } catch {
    case NonFatal(e) =>
      log.warn(s"Can not create a network for $tag", e)
      if (maxRetry == 0) throw e else createNetwork(maxRetry - 1)
  }

  private def ipamToString(network: Network): String =
    network
      .ipam()
      .config().asScala
      .map { n => s"subnet=${n.subnet()}, ip range=${n.ipRange()}" }
      .mkString(", ")

  private def buildNetworkConfig(): NetworkConfig = {
    val config = IpamConfig.create(networkPrefix, networkPrefix, ipForNode(0xE))
    val ipam = Ipam.builder()
      .driver("default")
      .config(Seq(config).asJava)
      .build()

    NetworkConfig.builder()
      .name(networkName)
      .ipam(ipam)
      .checkDuplicate(true)
      .build()
  }

  private def connectToNetwork(containerId: String, ip: String): AttachedNetwork = {
    client.connectToNetwork(
      innerNetwork.id(),
      NetworkConnection
        .builder()
        .containerId(containerId)
        .endpointConfig(endpointConfigFor(ip))
        .build()
    )
    waitForNetwork(containerId)
  }

  @tailrec private def waitForNetwork(containerId: String, maxTry: Int = 5): AttachedNetwork = {
    def errMsg = s"Container $containerId has not connected to the network ${innerNetwork.name()}"
    val containerInfo = client.inspectContainer(containerId)
    val networks = containerInfo.networkSettings().networks().asScala
    if (networks.contains(innerNetwork.name())) {
      networks(innerNetwork.name())
    } else if (maxTry > 0) {
      blocking(Thread.sleep(1000))
      log.debug(s"$errMsg, retrying. Max tries = $maxTry")
      waitForNetwork(containerId, maxTry - 1)
    } else {
      throw new IllegalStateException(errMsg)
    }
  }

  private def endpointConfigFor(ip: String): EndpointConfig =
    EndpointConfig.builder()
      .ipAddress(ip)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
      .build()

  private def ipForNode(nodeNumber: Int): String = {
    val addressBytes = Ints.toByteArray(nodeNumber & 0xF | networkSeed)
    InetAddress.getByAddress(addressBytes).getHostAddress
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
      client.logs(
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

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId, node.nodeInfo.networkIpAddress)

  def cleanUpDanglingResources(): Unit = {
    log.debug("Cleaning up Docker resources")

    // remove containers
    client.listContainers(ListContainersParam.allContainers()).asScala
      .filter(_.names.asScala.head.startsWith("/" + networkNamePrefix))
      .foreach(c => client.removeContainer(c.id, RemoveContainerParam.forceKill))

    // removes networks
    client.listNetworks(ListNetworksParam.customNetworks).asScala
      .filter(_.name().startsWith(networkNamePrefix))
      .foreach(n => client.removeNetwork(n.id))

    //remove images
    client.listImages(ListImagesParam.danglingImages()).asScala
      .filter(img => Option(img.labels()).exists(_.containsKey(dockerImageLabel)))
      .foreach(img => client.removeImage(img.id()))
  }

  def cleanupDanglingIfNeeded(): Unit = {
    val shouldCleanup = nodeConfigs.getOrElse[Boolean]("testing.integration.cleanupDocker", false)
    if (shouldCleanup) {
      cleanUpDanglingResources()
    }
  }
}

object Docker {

  val dockerImageLabel = "ergo-integration-tests"
  val networkNamePrefix: String = "ergo-itest-"

  val defaultConfigTemplate: Config = ConfigFactory.parseResources("template.conf")
  val nodeConfigs: Config = ConfigFactory.parseResources("nodes.conf").resolve()

  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def apply(owner: Class[_]): Docker = new Docker(tag = owner.getSimpleName)

}
