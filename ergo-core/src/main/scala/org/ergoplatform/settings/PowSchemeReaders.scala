package org.ergoplatform.settings

import com.typesafe.config.{Config, ConfigException}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.mining._

trait PowSchemeReaders {

  implicit val powSchemeReader: ValueReader[AutolykosPowScheme] = new ValueReader[AutolykosPowScheme] {
    override def read(cfg: Config, path: String): AutolykosPowScheme = {
      val schemeNameKey = s"$path.powType"
      val schemeName = cfg.getString(schemeNameKey)
      val n = cfg.as[Int](s"$path.n")
      val k = cfg.as[Int](s"$path.k")
      if (schemeName == "autolykos") {
        new AutolykosPowScheme(k, n)
      } else if (schemeName == "fake") {
        new DefaultFakePowScheme(k, n)
      } else {
        throw new ConfigException.BadValue(schemeNameKey, schemeName)
      }
    }
  }
}

