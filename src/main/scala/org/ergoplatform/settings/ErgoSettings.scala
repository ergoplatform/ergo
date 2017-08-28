package org.ergoplatform.settings

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import io.circe
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.ergoplatform.ErgoApp
import scorex.core.settings.Settings
import scorex.core.utils.ScorexLogging

case class ErgoSettings(directory: String,
                        nodeSettings: NodeConfigurationSettings,
                        scorexSettings: Settings)

object ErgoSettings extends ScorexLogging {

  val configPath: String = "ergo"

  def read(userConfigPath: Option[String]): ErgoSettings = {
    fromConfig(readConfigFromPath(userConfigPath: Option[String]))
  }

  private def fromConfig(config: Config): ErgoSettings = {
    val directory = config.as[String](s"$configPath.directory")
    val settingsFilename = config.as[String](s"$configPath.legacySettingsFilename")

    val nodeSettings = config.as[NodeConfigurationSettings](s"$configPath.node")
    val legacySettings = new Settings {
      override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename).ensuring(_.nonEmpty)
    }

    ErgoSettings(directory, nodeSettings, legacySettings)
  }

  private def readConfigFromPath(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        log.warn("NO CONFIGURATION FILE WAS PROVIDED. STARTING WITH DEFAULT SETTINGS FOR TESTNET!")
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("ergo")) {
          log.error("Malformed configuration file was provided! Aborting!")
          ErgoApp.forceStopApplication()
        }
        ConfigFactory
          .defaultOverrides()
          .withFallback(cfg)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
    }

    config
  }

}