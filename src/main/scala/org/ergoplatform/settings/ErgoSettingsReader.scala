package org.ergoplatform.settings

import java.io.{File, FileOutputStream}
import java.nio.channels.Channels
import ch.qos.logback.classic.{Level, LoggerContext}
import org.slf4j.{Logger, LoggerFactory}
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.ergoplatform.nodeView.state.StateType.Digest
import org.ergoplatform.ErgoApp
import scorex.util.ScorexLogging
import org.ergoplatform.settings.ErgoSettings.{configPath, scorexConfigPath}

import java.net.{InetAddress, URL}
import scala.util.Try

/**
  * Functions to read configs (ErgoSettings instances)
  */
object ErgoSettingsReader extends ScorexLogging
  with PowSchemeReaders
  with NodeConfigurationReaders
  with SettingsReaders {

  def read(args: Args = Args.empty): ErgoSettings = {
    fromConfig(readConfig(args), args.networkTypeOpt)
  }

  def fromConfig(config: Config, desiredNetworkTypeOpt: Option[NetworkType] = None): ErgoSettings = {
    val directory = config.as[String](s"$configPath.directory")
    val networkTypeName = config.as[String](s"$configPath.networkType")
    val networkType = NetworkType.fromString(networkTypeName)
      .getOrElse(throw new Error(s"Unknown `networkType = $networkTypeName`"))
    val nodeSettings = config.as[NodeConfigurationSettings](s"$configPath.node")
    val chainSettings = config.as[ChainSettings](s"$configPath.chain")
    val walletSettings = config.as[WalletSettings](s"$configPath.wallet")
    val cacheSettings = config.as[CacheSettings](s"$configPath.cache")
    val scorexSettings = config.as[ScorexSettings](scorexConfigPath)
    val votingTargets = VotingTargets.fromConfig(config)

    overrideLogLevel(scorexSettings.logging.level)

    if (nodeSettings.stateType == Digest && nodeSettings.mining) {
      log.error("Malformed configuration file was provided! Mining is not possible with digest state. Aborting!")
      ErgoApp.forceStopApplication()
    }

    consistentSettings(
      ErgoSettings(
        directory,
        networkType,
        chainSettings,
        nodeSettings,
        scorexSettings,
        walletSettings,
        cacheSettings,
        votingTargets
      ),
      desiredNetworkTypeOpt
    )
  }

  // Helper method to read user-provided `configFile` with network-specific `fallbackConfig`
  // to be used for default fallback values before reference.conf (which is the last resort)
  private def configWithOverrides(configFile: File, fallbackConfig: Option[File]) = {
    val firstFallBack = fallbackConfig.map(ConfigFactory.parseFile).getOrElse(ConfigFactory.empty())

    val cfg = ConfigFactory.parseFile(configFile)

    val keystorePath = "ergo.wallet.secretStorage.secretDir"

    // Check that user-provided Ergo directory exists and has write access (if provided at all)
    val userDirOpt = Try(cfg.getString("ergo.directory")).toOption
    userDirOpt.foreach { ergoDirName =>
      require(new File(s"$ergoDirName").canWrite, s"Folder $ergoDirName does not exist or not writable")
    }

    // Check that user-provided wallet secret directory exists and has read access (if provided at all)
    val walletKeystoreDirOpt = Try(cfg.getString(keystorePath)).toOption
    walletKeystoreDirOpt.foreach { secretDirName =>
      require(new File(s"$secretDirName").canRead, s"Folder $secretDirName does not exist or not readable")
    }

    val fullConfig = ConfigFactory
      .defaultOverrides()
      .withFallback(cfg)
      .withFallback(firstFallBack)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()

    // If user provided only ergo.directory but not ergo.wallet.secretStorage.secretDir in his config,
    // set ergo.wallet.secretStorage.secretDir like in reference.conf (so ergo.directory + "/wallet/keystore")
    // Otherwise, a user may have an issue, especially with Powershell it seems from reports.
    userDirOpt.map { userDir =>
      if(walletKeystoreDirOpt.isEmpty) {
        fullConfig.withValue(keystorePath, ConfigValueFactory.fromAnyRef(userDir + "/wallet/keystore"))
      } else {
        fullConfig
      }
    }.getOrElse(fullConfig)
  }

  private def readConfig(args: Args): Config = {

    val networkConfigFileOpt = args.networkTypeOpt
      .flatMap { networkType =>
        val confName = s"${networkType.verboseName}.conf"
        val classLoader = ClassLoader.getSystemClassLoader
        val destDir = System.getProperty("java.io.tmpdir") + "/"

        Option(classLoader.getResourceAsStream(confName))
          .map { stream =>
            val source = Channels.newChannel(stream)
            val fileOut = new File(destDir, confName)
            val dest = new FileOutputStream(fileOut)
            dest.getChannel.transferFrom(source, 0, Long.MaxValue)

            source.close()
            dest.close()

            sys.addShutdownHook {
              new File(destDir, confName).delete
            }

            fileOut
          }
      }

    val userConfigFileOpt = for {
      filePathOpt <- args.userConfigPathOpt
      file = new File(filePathOpt)
      if file.exists
    } yield file

    networkConfigFileOpt.flatMap(_ => args.networkTypeOpt).fold(log.warn("Running without network config"))(
      x => log.info(s"Running in ${x.verboseName} network mode"))

    (networkConfigFileOpt, userConfigFileOpt) match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case (Some(networkConfigFile), None) =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS!")
        ConfigFactory
          .defaultOverrides()
          .withFallback(ConfigFactory.parseFile(networkConfigFile))
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case (Some(networkConfigFile), Some(file)) =>
        configWithOverrides(file, Some(networkConfigFile))
      case (None, Some(file)) =>
        configWithOverrides(file, None)
      case (None, None) =>
        ConfigFactory.load()
    }
  }

  protected[settings] def invalidRestApiUrl(url: URL): Boolean =
    Try(url.toURI).map { uri =>
      val inetAddress = InetAddress.getByName(url.getHost)
      Option(uri.getQuery).exists(_.nonEmpty) ||
        Option(uri.getPath).exists(_.nonEmpty) ||
        Option(uri.getFragment).exists(_.nonEmpty) ||
        inetAddress.isAnyLocalAddress ||
        inetAddress.isLoopbackAddress ||
        inetAddress.isSiteLocalAddress
    }.getOrElse(false)

  private def consistentSettings(settings: ErgoSettings,
                                 desiredNetworkTypeOpt: Option[NetworkType]): ErgoSettings = {
    val nodeSettings = settings.nodeSettings
    if (nodeSettings.keepVersions < 0) {
      failWithError("nodeSettings.keepVersions should not be negative")
    } else if (!nodeSettings.verifyTransactions && !nodeSettings.stateType.requireProofs) {
      failWithError("Can not use UTXO state when nodeSettings.verifyTransactions is false")
    } else if (desiredNetworkTypeOpt.exists(_ != settings.networkType)) {
      failWithError(s"Malformed network config. Desired networkType is `${desiredNetworkTypeOpt.get}`, " +
        s"but one declared in config is `${settings.networkType}`")
    } else if(settings.networkType.isMainNet &&
      nodeSettings.mining &&
      !settings.chainSettings.reemission.checkReemissionRules) {
      failWithError(s"Mining is enabled, but ergo.chain.reemission.checkReemissionRules = false , set it to true")
    } else if (settings.scorexSettings.restApi.publicUrl.exists(invalidRestApiUrl)) {
      failWithError(s"scorex.restApi.publicUrl should not contain query, path or fragment and should not " +
        s"be local or loopback address : ${settings.scorexSettings.restApi.publicUrl.get}")
    } else if (settings.nodeSettings.utxoSettings.p2pUtxoSnapshots <= 0) {
      failWithError(s"p2pUtxoSnapshots <= 0, must be 1 at least")
    } else if (settings.nodeSettings.extraIndex && settings.nodeSettings.isFullBlocksPruned) {
      failWithError(s"Extra indexes could be enabled only if there is no blockchain pruning")
    } else if (nodeSettings.nipopowSettings.nipopowBootstrap &&
      !(nodeSettings.utxoSettings.utxoBootstrap || nodeSettings.blocksToKeep >= 0)) {
      failWithError("nodeSettings.popowBootstrap can be set only if " +
        "nodeSettings.utxoBootstrap is set or nodeSettings.blocksToKeep >=0")
    } else if (nodeSettings.nipopowSettings.nipopowBootstrap && settings.chainSettings.genesisId.isEmpty) {
      failWithError("nodeSettings.popowBootstrap is set but genesisId is not")
    } else {
      settings
    }
  }

  private def failWithError(msg: String): Nothing = {
    log.error(s"Stop application due to malformed configuration file: $msg")
    ErgoApp.forceStopApplication()
  }

  /**
   * Override the log level at runtime with values provided in config/user provided config.
   */
  private def overrideLogLevel(level: String): Unit = level match {
    case "TRACE" | "ERROR" | "INFO" | "WARN" | "DEBUG" =>
      log.info(s"Log level set to $level")
      val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      val root          = loggerContext.getLogger(Logger.ROOT_LOGGER_NAME)
      root.setLevel(Level.toLevel(level))
    case _ => log.warn("No log level configuration provided")
  }
}

