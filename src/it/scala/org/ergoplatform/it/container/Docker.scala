package org.ergoplatform.it.container

import java.io.{File, FileOutputStream}
import java.net.InetAddress
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Properties, UUID}
import cats.implicits._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.github.dockerjava.api.DockerClient
import com.github.dockerjava.api.command.{CreateContainerCmd, WaitContainerResultCallback}
import com.github.dockerjava.api.exception.NotFoundException
import com.github.dockerjava.api.model.{Bind, ContainerNetwork, ExposedPort, HostConfig, Network, PortBinding, Ports, Volume}
import com.github.dockerjava.api.model.Network.Ipam
import com.google.common.primitives.Ints
import com.github.dockerjava.core.{DefaultDockerClientConfig, DockerClientImpl}
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient
import com.github.dockerjava.transport.DockerHttpClient
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import net.ceedubs.ficus.Ficus._
import org.apache.commons.io.FileUtils
import org.asynchttpclient.Dsl.{config, _}
import org.ergoplatform.settings.NetworkType.{DevNet, DevNet60, MainNet, TestNet}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader, NetworkType}
import scorex.util.ScorexLogging

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Try}

class Docker(
  suiteConfig: Config                = ConfigFactory.empty,
  tag: String                        = "ergo_integration_test",
  localDataVolumeOpt: Option[String] = None
)(implicit ec: ExecutionContext)
  extends AutoCloseable
  with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(
    config()
      .setMaxConnections(50)
      .setMaxConnectionsPerHost(10)
      .setMaxRequestRetry(1)
      .setReadTimeout(10000)
      .setRequestTimeout(10000)
  )

  private val configStandart =
    DefaultDockerClientConfig.createDefaultConfigBuilder().build()

  private val httpDockerClient: DockerHttpClient = new ApacheDockerHttpClient.Builder()
    .dockerHost(configStandart.getDockerHost)
    .sslConfig(configStandart.getSSLConfig)
    .maxConnections(100)
    .connectionTimeout(java.time.Duration.ofSeconds(30))
    .responseTimeout(java.time.Duration.ofSeconds(45))
    .build()

  private val client: DockerClient =
    DockerClientImpl.getInstance(configStandart, httpDockerClient)
  private var nodeRepository                    = Seq.empty[Node]
  private var apiCheckerOpt: Option[ApiChecker] = None
  private val isStopped                         = new AtomicBoolean(false)

  // This should be called after client is ready but before network created.
  // This allows resource cleanup for the network if we are running out of them
  initBeforeStart()

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName       = Docker.networkNamePrefix + uuidShort
  private val networkSeed       = Random.nextInt(0x100000) << 4 | 0x0A000000

  private val networkPrefix =
    s"${InetAddress.getByAddress(Ints.toByteArray(networkSeed)).getHostAddress}/28"
  private val innerNetwork: Network = createNetwork(3)

  def nodes: Seq[Node] = nodeRepository

  def initBeforeStart(): Unit = {
    cleanupDanglingIfNeeded()
    sys.addShutdownHook {
      close()
    }
  }

  private def startNodes(
    networkType: NetworkType,
    nodeConfigs: List[Config],
    configEnrich: ExtraConfig
  ) = {
    log.trace(s"Starting ${nodeConfigs.size} containers")
    nodeConfigs
      .map(cfg => startNode(networkType, cfg, configEnrich))
      .sequence
      .map(waitForStartupBlocking)
  }

  def startDevNetNodes(
    nodeConfigs: List[Config],
    configEnrich: ExtraConfig = noExtraConfig
  ): Try[List[Node]] =
    startNodes(DevNet, nodeConfigs, configEnrich)

  def waitForStartupBlocking(nodes: List[Node]): List[Node] = {
    log.debug("Waiting for nodes to start")
    Await.result(waitForStartup(nodes), nodes.size * 90.seconds)
  }

  def waitForStartup(nodes: List[Node]): Future[List[Node]] = {
    Future.sequence(nodes map { _.waitForStartup })
  }

  def waitContainer(id: String): WaitContainerResultCallback = client.waitContainerCmd(id).start()

  def startOpenApiChecker(checkerInfo: ApiCheckerConfig): Try[ApiChecker] = Try {
    val ip: String          = ipForNode(999, networkSeed)
    val containerId: String = buildApiCheckerContainerCmd(checkerInfo, ip).exec().getId
    connectToNetwork(containerId, ip)
    client.startContainerCmd(containerId).exec()

    log.info(s"Started ApiChecker: $containerId")

    val checker: ApiChecker = ApiChecker(containerId, checkerInfo)
    apiCheckerOpt = Some(checker)
    checker
  }

  private def startNode(
    networkType: NetworkType,
    nodeSpecificConfig: Config,
    extraConfig: ExtraConfig,
    specialVolumeOpt: Option[(String, String)] = None
  ) = {
    val initialSettings    = buildErgoSettings(networkType, nodeSpecificConfig)
    val configuredNodeName = initialSettings.scorexSettings.network.nodeName
    val nodeNumber         = configuredNodeName.replace("node", "").toInt
    val ip                 = ipForNode(nodeNumber, networkSeed)
    val restApiPort        = initialSettings.scorexSettings.restApi.bindAddress.getPort
    val networkPort        = initialSettings.scorexSettings.network.bindAddress.getPort

    val nodeConfig: Config =
      enrichNodeConfig(networkType, nodeSpecificConfig, extraConfig, ip, networkPort)
    val settings: ErgoSettings = buildErgoSettings(networkType, nodeConfig)
    val containerBuilder: CreateContainerCmd =
      buildPeerContainerCmd(networkType, nodeConfig, settings, ip, specialVolumeOpt)
    val containerName = networkName + "-" + configuredNodeName + "-" + uuidShort

    Try {
      val containerId     = containerBuilder.withName(containerName).exec().getId
      val attachedNetwork = connectToNetwork(containerId, ip)
      client.startContainerCmd(containerId).exec()

      val containerInfo = client.inspectContainerCmd(containerId).exec()
      val ports         = containerInfo.getNetworkSettings.getPorts

      val nodeInfo = NodeInfo(
        hostRestApiPort      = extractHostPort(ports, restApiPort),
        hostNetworkPort      = extractHostPort(ports, networkPort),
        containerNetworkPort = networkPort,
        containerApiPort     = restApiPort,
        apiIpAddress         = containerInfo.getNetworkSettings.getIpAddress,
        networkIpAddress     = attachedNetwork.getIpAddress,
        containerId          = containerId
      )

      log.info(s"Started node: $nodeInfo")

      val node = new Node(settings, nodeInfo, http)
      nodeRepository = nodeRepository :+ node
      node
    } recoverWith {
      case e: NotFoundException =>
        Failure(
          new Exception(
            s"Error: docker image is missing. Run 'sbt it:test' to generate it.",
            e
          )
        )
    }
  }

  def startDevNetNode(
    nodeSpecificConfig: Config,
    extraConfig: ExtraConfig                   = noExtraConfig,
    specialVolumeOpt: Option[(String, String)] = None
  ): Try[Node] =
    startNode(DevNet, nodeSpecificConfig, extraConfig, specialVolumeOpt)

  def startTestNetNode(
    nodeSpecificConfig: Config,
    extraConfig: ExtraConfig                   = noExtraConfig,
    specialVolumeOpt: Option[(String, String)] = None
  ): Try[Node] =
    startNode(TestNet, nodeSpecificConfig, extraConfig, specialVolumeOpt)

  def startMainNetNodeYesImSure(
    nodeSpecificConfig: Config,
    extraConfig: ExtraConfig                   = noExtraConfig,
    specialVolumeOpt: Option[(String, String)] = None
  ): Try[Node] =
    startNode(MainNet, nodeSpecificConfig, extraConfig, specialVolumeOpt)

  private def buildErgoSettings(networkType: NetworkType, nodeConfig: Config) = {
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(defaultConfigTemplate(networkType))
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
    ErgoSettingsReader.fromConfig(actualConfig)
  }

  private def enrichNodeConfig(
    networkType: NetworkType,
    nodeConfig: Config,
    extraConfig: ExtraConfig,
    ip: String,
    port: Int
  ) = {
    val publicPeerConfig = nodeConfig //.withFallback(declaredAddressConfig(ip, port))
    val withPeerConfig = nodeRepository.headOption.fold(publicPeerConfig) { node =>
      knownPeersConfig(Seq(node.nodeInfo)).withFallback(publicPeerConfig)
    }
    val enrichedConfig =
      extraConfig(this, nodeConfig).fold(withPeerConfig)(_.withFallback(withPeerConfig))
    val actualConfig = enrichedConfig
      .withFallback(suiteConfig)
      .withFallback(defaultConfigTemplate(networkType))
    log.info(actualConfig.toString)
    actualConfig
  }

  private def buildApiCheckerContainerCmd(
    checkerInfo: ApiCheckerConfig,
    ip: String
  ): CreateContainerCmd = {
    val hostConfig: HostConfig = new HostConfig()
      .withBinds(
        new Bind(checkerInfo.specFilePath, new Volume("/opt/ergo/openapi.yaml")),
        new Bind(checkerInfo.paramsFilePath, new Volume("/opt/ergo/parameters.yaml"))
      )

    client
      .createContainerCmd(ApiCheckerImageStable)
      .withCmd(
        "openapi.yaml",
        "--api",
        s"http://${checkerInfo.apiAddressToCheck}",
        "--parameters",
        "parameters.yaml"
      )
      .withHostConfig(hostConfig)
      .withHostName(networkName)
      .withIpv4Address(ip)
  }

  private def buildPeerContainerCmd(
    networkType: NetworkType,
    nodeConfig: Config,
    settings: ErgoSettings,
    ip: String,
    specialVolumeOpt: Option[(String, String)]
  ) = {
    val restApiPort = settings.scorexSettings.restApi.bindAddress.getPort
    val networkPort = settings.scorexSettings.network.bindAddress.getPort
    val portBindings = List(
      new PortBinding(new Ports.Binding("0.0.0.0", null), new ExposedPort(restApiPort)),
      new PortBinding(new Ports.Binding("0.0.0.0", null), new ExposedPort(networkPort))
    )

    val oneGB: Long = 1024 * 1024 * 1024
    val memoryLimit = networkType match {
      case MainNet => 3 * oneGB
      case _       => oneGB
    }

    val hostConfig: HostConfig = specialVolumeOpt
      .map {
        case (lv, rv) =>
          new HostConfig().withBinds(
            new Bind(lv, new Volume(rv))
          )
      }
      .getOrElse(new HostConfig())
      .withPortBindings(portBindings.asJava)
      .withMemory(memoryLimit)

    val configCommandLine = renderProperties(asProperties(nodeConfig))

    val networkTypeCmdOption = networkType match {
      case MainNet => "--mainnet"
      case TestNet => "--testnet"
      case DevNet  => ""
      case DevNet60  => ""
    }

    val miscCmdOptions = networkType match {
      case MainNet => "-Xmx2G"
      case _       => ""
    }

    val shellCmd = "echo Options: $OPTS; java $OPTS -Dlibrary.leveldbjni.path=/opt/ergo -jar " +
      s"$miscCmdOptions /opt/ergo/ergo.jar $networkTypeCmdOption -c /opt/ergo/${networkType.verboseName}Template.conf"

    client
      .createContainerCmd(ErgoImageLatest)
      .withExposedPorts(
        List(ExposedPort.tcp(restApiPort), ExposedPort.tcp(networkPort)).asJava
      )
      .withHostConfig(hostConfig)
      .withHostName(networkName)
      .withIpv4Address(ip)
      .withEnv(s"OPTS=$configCommandLine")
      .withEntrypoint("sh", "-c", shellCmd)
  }

  private def createNetwork(maxRetry: Int): Network =
    try {
      val networkOpt =
        client.listNetworksCmd().withNameFilter(networkName).exec().asScala.headOption
      networkOpt match {
        case Some(network) =>
          log.info(
            s"Network ${network.getName} (id: ${network.getId}) is created for $tag, " +
            s"ipam: ${ipamToString(network)}"
          )
          network
        case None =>
          log.debug(s"Creating network $networkName for $tag")
          val r = client
            .createNetworkCmd()
            .withName(networkName)
            .withAttachable(true)
            .withIpam(buildNetworkConfigIpam())
            .withCheckDuplicate(true)
            .exec()
          Option(r.getWarnings).foreach(_.foreach(log.warn(_)))
          createNetwork(maxRetry - 1) //proceed to check if created
      }
    } catch {
      case NonFatal(e) =>
        log.warn(s"Can not create a network for $tag", e)
        if (maxRetry == 0) throw e else createNetwork(maxRetry - 1)
    }

  private def buildNetworkConfigIpam(): Ipam = {
    val config = new Ipam.Config()
      .withSubnet(networkPrefix)
      .withIpRange(networkPrefix)
      .withGateway(ipForNode(0xE, networkSeed))
    new Ipam()
      .withDriver("default")
      .withConfig(config)
  }

  private def connectToNetwork(containerId: String, ip: String): ContainerNetwork = {
    client
      .connectToNetworkCmd()
      .withNetworkId(innerNetwork.getId)
      .withContainerId(containerId)
      .withContainerNetwork(new ContainerNetwork().withIpv4Address(ip))
      .exec()

    waitForNetwork(containerId)
  }

  @tailrec private def waitForNetwork(
    containerId: String,
    maxTry: Int = 5
  ): ContainerNetwork = {
    def errMsg =
      s"Container $containerId has not connected to the network ${innerNetwork.getName}"
    val containerInfo = client.inspectContainerCmd(containerId).exec()
    val networks      = containerInfo.getNetworkSettings.getNetworks.asScala
    if (networks.contains(innerNetwork.getName)) {
      networks(innerNetwork.getName)
    } else if (maxTry > 0) {
      blocking(Thread.sleep(1000))
      log.debug(s"$errMsg, retrying. Max tries = $maxTry")
      waitForNetwork(containerId, maxTry - 1)
    } else {
      throw new IllegalStateException(errMsg)
    }
  }

  def stopNode(node: Node, secondsToWait: Int): Unit =
    stopNode(node.containerId, secondsToWait)

  def stopNode(containerId: String, secondsToWait: Int = 5): Unit = {
    nodeRepository = nodeRepository.filterNot(_.containerId == containerId)
    client.stopContainerCmd(containerId).withTimeout(secondsToWait).exec()
  }

  def forceStopNode(containerId: String): Unit = {
    nodeRepository = nodeRepository.filterNot(_.containerId == containerId)
    client.removeContainerCmd(containerId).withForce(true).exec()
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodeRepository foreach { node =>
        node.close()
        client.stopContainerCmd(node.containerId).withTimeout(0).exec()
      }
      http.close()

      saveNodeLogs()

      apiCheckerOpt.foreach { checker =>
        saveLogs(checker.containerId, "openapi-checker")
        client.removeContainerCmd(checker.containerId).withForce(true).exec()
      }

      nodeRepository foreach { node =>
        client.removeContainerCmd(node.containerId).withForce(true).exec()
      }
      client.removeNetworkCmd(innerNetwork.getId).exec()
      client.close()

      localDataVolumeOpt.foreach { path =>
        val dataVolume = new File(path)
        FileUtils.forceDeleteOnExit(dataVolume)
      }
    }
  }

  private def saveLogs(containerId: String, tag: String): Unit = {
    val logDir: Path = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)

    val fileName: String = s"$tag-$containerId"
    val logFile: File    = logDir.resolve(s"$fileName.log").toFile
    log.info(s"Writing logs of $tag-$containerId to ${logFile.getAbsolutePath}")

    val fileStream: FileOutputStream = new FileOutputStream(logFile, false)
    client
      .logContainerCmd(containerId)
      .withTimestamps(true)
      .withFollowStream(true)
      .withStdOut(true)
      .withStdErr(true)
      .start()
      .onStart(fileStream)
  }

  private def saveNodeLogs(): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    nodeRepository.foreach { node =>
      import node.nodeInfo.containerId

      val fileName = if (tag.isEmpty) containerId else s"$tag-$containerId"
      val logFile  = logDir.resolve(s"$fileName.log").toFile
      log.info(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

      val fileStream = new FileOutputStream(logFile, false)
      client
        .logContainerCmd(containerId)
        .withTimestamps(true)
        .withFollowStream(true)
        .withStdOut(true)
        .withStdErr(true)
        .start()
        .onStart(fileStream)

    }
  }

  def disconnectFromNetwork(containerId: String): Unit =
    client
      .disconnectFromNetworkCmd()
      .withContainerId(containerId)
      .withNetworkId(innerNetwork.getId)
      .exec()

  def disconnectFromNetwork(node: Node): Unit =
    disconnectFromNetwork(node.nodeInfo.containerId)

  def connectToNetwork(node: Node): Unit =
    connectToNetwork(node.nodeInfo.containerId, node.nodeInfo.networkIpAddress)

  def cleanupDanglingResources(): Unit = {
    log.debug("Cleaning up Docker resources")

    // remove containers
    client
      .listContainersCmd()
      .exec()
      .asScala
      .filter(_.getNames.head.startsWith("/" + networkNamePrefix))
      .foreach(c => client.removeContainerCmd(c.getId).withForce(true).exec())

    // removes networks
    client
      .listNetworksCmd()
      .exec()
      .asScala
      .filter(_.getName.startsWith(networkNamePrefix))
      .foreach(n => client.removeNetworkCmd(n.getId).exec())

    //remove images
    client
      .listImagesCmd()
      .withDanglingFilter(true)
      .exec()
      .asScala
      .filter(img => Option(img.getLabels).exists(_.containsKey(dockerImageLabel)))
      .foreach(img => client.removeImageCmd(img.getId).exec())
  }

  def cleanupDanglingIfNeeded(): Unit = {
    val shouldCleanup =
      nodesJointConfig.getOrElse[Boolean]("testing.integration.cleanupDocker", false)
    if (shouldCleanup) {
      cleanupDanglingResources()
    }
  }
}

object Docker extends IntegrationTestConstants {

  val ErgoImageLatest: String       = "org.ergoplatform/ergo"
  val ApiCheckerImageLatest: String = "andyceo/openapi-checker"
  val ApiCheckerImageStable: String = "andyceo/openapi-checker:0.1.0-openapi-core-0.5.0" // not present in docker anymore

  val dockerImageLabel          = "ergo-integration-tests"
  val networkNamePrefix: String = "ergo-itest-"

  type ExtraConfig = (Docker, Config) => Option[Config]

  def noExtraConfig: ExtraConfig = (_, _) => None

  private val jsonMapper  = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def ipForNode(nodeNumber: Int, networkSeed: Int): String = {
    val addressBytes = Ints.toByteArray(nodeNumber & 0xF | networkSeed)
    InetAddress.getByAddress(addressBytes).getHostAddress
  }

  def ipamToString(network: Network): String =
    network.getIpam.getConfig.asScala
      .map { n =>
        s"subnet=${n.getSubnet}, ip range=${n.getIpRange}"
      }
      .mkString(", ")

  def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  def renderProperties(props: Properties): String =
    props.asScala.map {
      case (k, v) if v.split(" ").length > 1 => s"-D$k=${v.split(" ").mkString("_")}"
      case (k, v)                            => s"-D$k=$v"
    } mkString " "

  def extractHostPort(ports: Ports, containerPort: Int): Int =
    ports.getBindings
      .get(ExposedPort.tcp(containerPort))(0)
      .getHostPortSpec
      .toInt
}
