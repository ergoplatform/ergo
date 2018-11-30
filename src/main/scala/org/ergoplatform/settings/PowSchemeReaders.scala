package org.ergoplatform.settings

import com.typesafe.config.{Config, ConfigException}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.mining._


trait PowSchemeReaders {

  val readers: Seq[PowSchemeReader[_ <: AutolykosPowScheme]] = Seq(
    AutolykosPowSchemeReader,
    FakePowSchemeReader
  )

  implicit val powSchemeReader: ValueReader[AutolykosPowScheme] = { (cfg, path) =>
    val schemeNameKey = s"$path.powType"
    val schemeName = cfg.getString(schemeNameKey)
    val schemeReader = readers.find(_.schemeName == schemeName)
      .getOrElse(throw new ConfigException.BadValue(schemeNameKey, schemeName))
    schemeReader.read(cfg, path)
  }
}

sealed trait PowSchemeReader[T <: AutolykosPowScheme] {
  def schemeName: String

  def read(config: Config, path: String): T
}

object AutolykosPowSchemeReader extends PowSchemeReader[AutolykosPowScheme] {
  val schemeName = "autolykos"

  def read(config: Config, path: String): AutolykosPowScheme = {
    val n = config.as[Int](s"$path.n")
    val k = config.as[Int](s"$path.k")
    new AutolykosPowScheme(k, n)
  }
}

object FakePowSchemeReader extends PowSchemeReader[AutolykosPowScheme] {
  val schemeName = "fake"

  def read(config: Config, path: String): AutolykosPowScheme = DefaultFakePowScheme
}
