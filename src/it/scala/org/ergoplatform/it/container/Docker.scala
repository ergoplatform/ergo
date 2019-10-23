package org.ergoplatform.it.container

import java.io.{File, FileOutputStream}
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, UUID, List => JList, Map => JMap}

import cats.implicits._
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
import org.apache.commons.io.FileUtils
import org.asynchttpclient.Dsl.{config, _}
import org.ergoplatform.settings.NetworkType.{DevNet, MainNet, TestNet}
import org.ergoplatform.settings.{ErgoSettings, NetworkType}
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Try}

// scalastyle:off number.of.methods
class Docker(suiteConfig: Config = ConfigFactory.empty,
             tag: String = "ergo_integration_test",
             localDataVolumeOpt: Option[String] = None)
            (implicit ec: ExecutionContext) extends AutoCloseable with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setRequestTimeout(10000))

  private val client = DefaultDockerClient.fromEnv().build()
  private var nodeRepository = Seq.empty[Node]
  private var apiCheckerOpt: Option[ApiChecker] = None
  private val isStopped = new AtomicBoolean(false)

  // This should be called after client is ready but before network created.
  // This allows resource cleanup for the network if we are running out of them
  initBeforeStart()

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000
  private val networkPrefix = s"${InetAddress.getByAddress(Ints.toByteArray(networkSeed)).getHostAddress}/28"
  private val innerNetwork: Network = createNetwork(3)

  def nodes: Seq[Node] = nodeRepository

  def initBeforeStart(): Unit = {
    cleanupDanglingIfNeeded()
    sys.addShutdownHook {
      close()
    }
  }

  private def startNodes(networkType: NetworkType,
                         nodeConfigs: List[Config],
                         configEnrich: ExtraConfig = noExtraConfig) = {
    log.trace(s"Starting ${nodeConfigs.size} containers")
    nodeConfigs.map(cfg => startNode(networkType, cfg, configEnrich))
      .sequence
      .map(waitForStartupBlocking)
  }

  def startDevNetNodes(nodeConfigs: List[Config],
                       configEnrich: ExtraConfig = noExtraConfig): Try[List[Node]] =
    startNodes(DevNet, nodeConfigs, configEnrich)

  def waitForStartupBlocking(nodes: List[Node]): List[Node] = {
    log.debug("Waiting for nodes to start")
    Await.result(waitForStartup(nodes), nodes.size * 90.seconds)
  }

  def waitForStartup(nodes: List[Node]): Future[List[Node]] = {
    Future.sequence(nodes map { _.waitForStartup })
  }

  def waitContainer(id: String): ContainerExit = client.waitContainer(id)

  def startOpenApiChecker(checkerInfo: ApiCheckerConfig): Try[ApiChecker] = Try {
    client.pull(ApiCheckerImageStable)

    val ip: String = ipForNode(999, networkSeed)
    val containerId: String = client.createContainer(buildApiCheckerContainerConfig(checkerInfo, ip)).id
    connectToNetwork(containerId, ip)
    client.startContainer(containerId)

    log.info(s"Started ApiChecker: $containerId")

    val checker: ApiChecker = ApiChecker(containerId, checkerInfo)
    apiCheckerOpt = Some(checker)
    checker
  }

  private def startNode(networkType: NetworkType,
                        nodeSpecificConfig: Config,
                        extraConfig: ExtraConfig = noExtraConfig,
                        specialVolumeOpt: Option[(String, String)] = None) = {
    val initialSettings = buildErgoSettings(networkType, nodeSpecificConfig)
    val configuredNodeName = initialSettings.scorexSettings.network.nodeName
    val nodeNumber = configuredNodeName.replace("node", "").toInt
    val ip = ipForNode(nodeNumber, networkSeed)
    val restApiPort = initialSettings.scorexSettings.restApi.bindAddress.getPort
    val networkPort = initialSettings.scorexSettings.network.bindAddress.getPort

    val nodeConfig: Config = enrichNodeConfig(networkType, nodeSpecificConfig, extraConfig, ip, networkPort)
    val settings: ErgoSettings = buildErgoSettings(networkType, nodeConfig)
    val containerConfig: ContainerConfig = buildPeerContainerConfig(networkType, nodeConfig, settings,
      ip, specialVolumeOpt)
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
        containerApiPort = restApiPort,
        apiIpAddress = containerInfo.networkSettings().ipAddress(),
        networkIpAddress = attachedNetwork.ipAddress(),
        containerId = containerId)

      log.info(s"Started node: $nodeInfo")

      val node = new Node(settings, nodeInfo, http)
      nodeRepository = nodeRepository :+ node
      node
    } recoverWith {
      case e: ImageNotFoundException =>
        Failure(new Exception(s"Error: docker image is missing. Run 'sbt it:test' to generate it.", e))
    }
  }

  def startDevNetNode(nodeSpecificConfig: Config,
                      extraConfig: ExtraConfig = noExtraConfig,
                      specialVolumeOpt: Option[(String, String)] = None): Try[Node] =
    startNode(DevNet, nodeSpecificConfig, extraConfig, specialVolumeOpt)

  def startTestNetNode(nodeSpecificConfig: Config,
                      extraConfig: ExtraConfig = noExtraConfig,
                      specialVolumeOpt: Option[(String, String)] = None): Try[Node] =
    startNode(TestNet, nodeSpecificConfig, extraConfig, specialVolumeOpt)

  def startMainNetNodeYesImSure(nodeSpecificConfig: Config,
                                extraConfig: ExtraConfig = noExtraConfig,
                                specialVolumeOpt: Option[(String, String)] = None): Try[Node] =
    startNode(MainNet, nodeSpecificConfig, extraConfig, specialVolumeOpt)

  private def buildErgoSettings(networkType: NetworkType, nodeConfig: Config) = {
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(defaultConfigTemplate(networkType))
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
    ErgoSettings.fromConfig(actualConfig)
  }

  private def enrichNodeConfig(networkType: NetworkType,
                               nodeConfig: Config,
                               extraConfig: ExtraConfig,
                               ip: String,
                               port: Int) = {
    val publicPeerConfig = nodeConfig//.withFallback(declaredAddressConfig(ip, port))
    val withPeerConfig = nodeRepository.headOption.fold(publicPeerConfig) { node =>
      knownPeersConfig(Seq(node.nodeInfo)).withFallback(publicPeerConfig)
    }
    val enrichedConfig = extraConfig(this, nodeConfig).fold(withPeerConfig)(_.withFallback(withPeerConfig))
    val actualConfig = enrichedConfig.withFallback(suiteConfig)
      .withFallback(defaultConfigTemplate(networkType))
    actualConfig
  }

  private def buildApiCheckerContainerConfig(checkerInfo: ApiCheckerConfig, ip: String): ContainerConfig = {
    val hostConfig: HostConfig = HostConfig.builder()
      .appendBinds(s"${checkerInfo.specFilePath}:/app/openapi.yaml", s"${checkerInfo.paramsFilePath}:/app/parameters.yaml")
      .build()

    val networkingConfig: ContainerConfig.NetworkingConfig = ContainerConfig.NetworkingConfig
      .create(Map(networkName -> endpointConfigFor(ip)).asJava)

    ContainerConfig.builder()
      .image(ApiCheckerImageStable)
      .cmd("openapi.yaml", "--api", s"http://${checkerInfo.apiAddressToCheck}", "--parameters", "parameters.yaml")
      .networkingConfig(networkingConfig)
      .hostConfig(hostConfig)
      .build()
  }

  private def buildPeerContainerConfig(networkType: NetworkType,
                                       nodeConfig: Config,
                                       settings: ErgoSettings,
                                       ip: String,
                                       specialVolumeOpt: Option[(String, String)] = None): ContainerConfig = {
    val restApiPort = settings.scorexSettings.restApi.bindAddress.getPort
    val networkPort = settings.scorexSettings.network.bindAddress.getPort
    val portBindings = new ImmutableMap.Builder[String, JList[PortBinding]]()
      .put(restApiPort.toString, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(networkPort.toString, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .build()

    val oneGB: Long = 1024 * 1024 * 1024
    val memoryLimit = networkType match {
      case MainNet => 2 * oneGB
      case _ => oneGB
    }

    val hostConfig = specialVolumeOpt
      .map { case (lv, rv) =>
        HostConfig.builder()
          .appendBinds(s"$lv:$rv")
      }
      .getOrElse(HostConfig.builder())
      .portBindings(portBindings)
      .memory(memoryLimit)
      .build()

    val networkingConfig = ContainerConfig.NetworkingConfig
      .create(Map(networkName -> endpointConfigFor(ip)).asJava)

    val configCommandLine = renderProperties(asProperties(nodeConfig))

    val networkTypeCmdOption = networkType match {
      case MainNet => "--mainnet"
      case TestNet => "--testnet"
      case DevNet => ""
    }

    val miscCmdOptions = networkType match {
      case MainNet => "-Xmx2G"
      case _ => ""
    }

    val shellCmd = "echo Options: $OPTS; java $OPTS -jar " +
      s"$miscCmdOptions /opt/ergo/ergo.jar $networkTypeCmdOption -c /opt/ergo/${networkType.verboseName}Template.conf"

    ContainerConfig.builder()
      .image(ErgoImageLatest)
      .exposedPorts(restApiPort.toString, networkPort.toString)
      .networkingConfig(networkingConfig)
      .hostConfig(hostConfig)
      .env(s"OPTS=$configCommandLine")
      .entrypoint("sh", "-c", shellCmd)
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

  private def buildNetworkConfig(): NetworkConfig = {
    val config = IpamConfig.create(networkPrefix, networkPrefix, ipForNode(0xE, networkSeed))
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

  def stopNode(node: Node, secondsToWait: Int): Unit = stopNode(node.containerId, secondsToWait)

  def stopNode(containerId: String, secondsToWait: Int = 5): Unit = {
    nodeRepository = nodeRepository.filterNot(_.containerId == containerId)
    client.stopContainer(containerId, secondsToWait)
  }

  def forceStopNode(containerId: String): Unit = {
    nodeRepository = nodeRepository.filterNot(_.containerId == containerId)
    client.removeContainer(containerId, RemoveContainerParam.forceKill())
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodeRepository foreach { node =>
        node.close()
        client.stopContainer(node.containerId, 0)
      }
      http.close()

      saveNodeLogs()

      apiCheckerOpt.foreach { checker =>
        saveLogs(checker.containerId, "openapi-checker")
        client.removeContainer(checker.containerId, RemoveContainerParam.forceKill())
      }

      nodeRepository foreach { node =>
        client.removeContainer(node.containerId, RemoveContainerParam.forceKill())
      }
      client.removeNetwork(innerNetwork.id())
      client.close()

      localDataVolumeOpt.foreach { path =>
        val dataVolume = new File(path)
        FileUtils.deleteDirectory(dataVolume)
      }
    }
  }

  private def saveLogs(containerId: String, tag: String): Unit = {
    val logDir: Path = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)

    val fileName: String = s"$tag-$containerId"
    val logFile: File = logDir.resolve(s"$fileName.log").toFile
    log.info(s"Writing logs of $tag-$containerId to ${logFile.getAbsolutePath}")

    val fileStream: FileOutputStream = new FileOutputStream(logFile, false)
    client.logs(
      containerId,
      DockerClient.LogsParam.timestamps(),
      DockerClient.LogsParam.follow(),
      DockerClient.LogsParam.stdout(),
      DockerClient.LogsParam.stderr()
    )
      .attach(fileStream, fileStream)
  }

  private def saveNodeLogs(): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    nodeRepository.foreach { node =>
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

  def cleanupDanglingResources(): Unit = {
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
    val shouldCleanup = nodesJointConfig.getOrElse[Boolean]("testing.integration.cleanupDocker", false)
    if (shouldCleanup) {
      cleanupDanglingResources()
    }
  }
}
// scalastyle:on number.of.methods

object Docker extends IntegrationTestConstants {

  val ErgoImageLatest: String = "org.ergoplatform/ergo:latest"
  val ApiCheckerImageLatest: String = "andyceo/openapi-checker:latest"
  val ApiCheckerImageStable: String = "andyceo/openapi-checker:0.1.0-openapi-core-0.5.0"

  val dockerImageLabel = "ergo-integration-tests"
  val networkNamePrefix: String = "ergo-itest-"

  type ExtraConfig = (Docker, Config) => Option[Config]

  def noExtraConfig: ExtraConfig = (_, _) => None


  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def endpointConfigFor(ip: String): EndpointConfig =
    EndpointConfig.builder()
      .ipAddress(ip)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
      .build()

  def ipForNode(nodeNumber: Int, networkSeed: Int): String = {
    val addressBytes = Ints.toByteArray(nodeNumber & 0xF | networkSeed)
    InetAddress.getByAddress(addressBytes).getHostAddress
  }

  def ipamToString(network: Network): String =
    network
      .ipam()
      .config().asScala
      .map { n => s"subnet=${n.subnet()}, ip range=${n.ipRange()}" }
      .mkString(", ")

  def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  def renderProperties(props: Properties): String =
    props.asScala.map {
      case (k, v) if v.split(" ").length > 1 => s"-D$k=${v.split(" ").mkString("_")}"
      case (k, v) => s"-D$k=$v"
    } mkString " "

  def extractHostPort(portBindingMap: JMap[String, JList[PortBinding]], containerPort: Int): Int =
    portBindingMap.get(s"$containerPort/tcp").get(0).hostPort().toInt
}
