package org.ergoplatform.settings

import java.io.{File, FileOutputStream}
import java.nio.channels.Channels

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.ergoplatform.mining.groupElemFromBytes
import org.ergoplatform.nodeView.state.StateType.Digest
import org.ergoplatform.{ErgoAddressEncoder, ErgoApp, P2PKAddress}
import scorex.core.settings.{ScorexSettings, SettingsReaders}
import scorex.util.ScorexLogging
import scorex.util.encode.Base16
import sigmastate.basics.DLogProtocol.ProveDlog

import scala.util.Try


case class ErgoSettings(directory: String,
                        networkType: NetworkType,
                        chainSettings: ChainSettings,
                        nodeSettings: NodeConfigurationSettings,
                        scorexSettings: ScorexSettings,
                        walletSettings: WalletSettings,
                        cacheSettings: CacheSettings,
                        votingTargets: VotingTargets = VotingTargets.empty) {

  val addressEncoder = ErgoAddressEncoder(chainSettings.addressPrefix)

  val miningRewardDelay: Int = chainSettings.monetary.minerRewardDelay

  val miningPubKey: Option[ProveDlog] = nodeSettings.miningPubKeyHex
    .flatMap { str =>
      val keyBytes = Base16.decode(str)
        .getOrElse(throw new Error(s"Failed to parse `miningPubKeyHex = ${nodeSettings.miningPubKeyHex}`"))
      Try(ProveDlog(groupElemFromBytes(keyBytes)))
        .orElse(addressEncoder.fromString(str).collect { case p2pk: P2PKAddress => p2pk.pubkey })
        .toOption
    }

}

object ErgoSettings extends ScorexLogging
  with PowSchemeReaders
  with NodeConfigurationReaders
  with SettingsReaders {

  val configPath: String = "ergo"
  val scorexConfigPath: String = "scorex"

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
    val firstFallBack = fallbackConfig.map(ConfigFactory.parseFile).getOrElse(ConfigFactory.defaultApplication())

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

  private def consistentSettings(settings: ErgoSettings,
                                 desiredNetworkTypeOpt: Option[NetworkType]): ErgoSettings = {
    if (settings.nodeSettings.keepVersions < 0) {
      failWithError("nodeSettings.keepVersions should not be negative")
    } else if (!settings.nodeSettings.verifyTransactions && !settings.nodeSettings.stateType.requireProofs) {
      failWithError("Can not use UTXO state when nodeSettings.verifyTransactions is false")
    } else if (desiredNetworkTypeOpt.exists(_ != settings.networkType)) {
      failWithError(s"Malformed network config. Desired networkType is `${desiredNetworkTypeOpt.get}`, " +
        s"but one declared in config is `${settings.networkType}`")
    } else {
      settings
    }
  }

  private def failWithError(msg: String): Nothing = {
    log.error(s"Stop application due to malformed configuration file: $msg")
    ErgoApp.forceStopApplication()
  }

}
