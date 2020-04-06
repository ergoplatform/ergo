package org.ergoplatform.settings

import java.io.{File, FileOutputStream}
import java.nio.channels.Channels
import com.typesafe.config.{Config, ConfigFactory}
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
                        testingSettings: TestingSettings,
                        nodeSettings: NodeConfigurationSettings,
                        scorexSettings: ScorexSettings,
                        walletSettings: WalletSettings,
                        cacheSettings: CacheSettings,
                        bootstrapSettingsOpt: Option[BootstrapSettings] = None,
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
    val bootstrappingSettingsOpt = config.as[Option[BootstrapSettings]](s"$configPath.bootstrap")
    val chainSettings = config.as[ChainSettings](s"$configPath.chain")
    val testingSettings = config.as[TestingSettings](s"$configPath.testing")
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
        testingSettings,
        nodeSettings,
        scorexSettings,
        walletSettings,
        cacheSettings,
        bootstrappingSettingsOpt,
        votingTargets
      ),
      desiredNetworkTypeOpt
    )
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

            sys.addShutdownHook { new File(destDir, confName).delete }

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
        val cfg = ConfigFactory.parseFile(file)
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.parseFile(networkConfigFile))
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
      case (None, Some(file)) =>
        val cfg = ConfigFactory.parseFile(file)
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
      case _ =>
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
