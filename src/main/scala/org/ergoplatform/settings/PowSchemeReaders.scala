package org.ergoplatform.settings

import com.typesafe.config.{Config, ConfigException}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.mining._


trait PowSchemeReaders {

  val readers: Seq[PowSchemeReader[_ <: AutoleakusPowScheme]] = Seq(
    AutoleakusPowSchemeReader,
    FakePowSchemeReader
  )

  implicit val powSchemeReader: ValueReader[AutoleakusPowScheme] = { (cfg, path) =>
    val schemeNameKey = s"$path.powType"
    val schemeName = cfg.getString(schemeNameKey)
    val schemeReader = readers.find(_.schemeName == schemeName)
      .getOrElse(throw new ConfigException.BadValue(schemeNameKey, schemeName))
    schemeReader.read(cfg, path)
  }
}

sealed trait PowSchemeReader[T <: AutoleakusPowScheme] {
  def schemeName: String

  def read(config: Config, path: String): T
}

object AutoleakusPowSchemeReader extends PowSchemeReader[AutoleakusPowScheme] {
  val schemeName = "autoleakus"

  def read(config: Config, path: String): AutoleakusPowScheme = {
    val N = config.as[Int](s"$path.N").toChar
    val k = config.as[Int](s"$path.k").toChar
    new AutoleakusPowScheme(k, N)
  }
}

object FakePowSchemeReader extends PowSchemeReader[AutoleakusPowScheme] {
  val schemeName = "fake"

  def read(config: Config, path: String): AutoleakusPowScheme = DefaultFakePowScheme
}
