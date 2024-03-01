package org.ergoplatform.settings

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import java.io.File

object ChainSettingsReader extends PowSchemeReaders with ModifierIdReader with SettingsReaders{
  def read(path: String): Option[ChainSettings] = {
    val file = new File(path)
    if (file.exists) {
      val cfg = ConfigFactory.parseFile(file)
      val fallback = ConfigFactory.parseFile(new File("src/main/resources/application.conf"))
      val network = ConfigFactory.parseFile(new File("src/main/resources/testnet.conf"))
      val fullConfig = ConfigFactory
        .defaultOverrides()
        .withFallback(cfg) //order
        .withFallback(network) //matters
        .withFallback(fallback) //here
        .resolve()

      val chainSettings = fullConfig.as[ChainSettings]("ergo.chain")
      Some(chainSettings)
    } else None
  }
}
