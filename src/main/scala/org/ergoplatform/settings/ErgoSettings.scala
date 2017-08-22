package org.ergoplatform.settings

import com.typesafe.config.Config
import io.circe
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import scorex.core.settings.Settings

case class ErgoSettings(directory: String,
                        nodeSettings: NodeConfigurationSettings,
                        legacySettings: Settings)

object ErgoSettings {

  val configPath: String = "ergo"

  def fromConfig(config: Config): ErgoSettings = {
    val directory = config.as[String](s"$configPath.directory")
    val settingsFilename = config.as[String](s"$configPath.legacySettingsFilename")

    val nodeSettings = config.as[NodeConfigurationSettings](s"$configPath.node")
    val legacySettings = new Settings {
      override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename).ensuring(_.nonEmpty)
    }

    ErgoSettings(directory, nodeSettings, legacySettings)
  }
}