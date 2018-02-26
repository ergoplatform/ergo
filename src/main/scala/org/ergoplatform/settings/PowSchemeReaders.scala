package org.ergoplatform.settings

import com.typesafe.config.{Config, ConfigException}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.mining._


trait PowSchemeReaders {

  val readers: Seq[PowSchemeReader[_ <: PoWScheme]] = Seq(
    EquihashPowSchemeReader,
    FakePowSchemeReader
  )

  implicit val powSchemeReader: ValueReader[PoWScheme] =  { (cfg, path) =>
    val schemeNameKey = s"$path.powType"
    val schemeName = cfg.getString(schemeNameKey)
    val schemeReader = readers.find(_.schemeName == schemeName)
                              .getOrElse(throw new ConfigException.BadValue(schemeNameKey, schemeName))
    schemeReader.read(cfg, path)
  }
}

sealed trait PowSchemeReader[T <: PoWScheme] {
  def schemeName: String
  def read(config: Config, path: String): T
}

object EquihashPowSchemeReader extends PowSchemeReader[EquihashPowScheme] {
  val schemeName = "equihash"
  def read(config: Config, path: String): EquihashPowScheme = {
    val n = config.as[Int](s"$path.n").toChar
    val k = config.as[Int](s"$path.k").toChar
    new EquihashPowScheme(n, k)
  }
}

object FakePowSchemeReader extends PowSchemeReader[FakePowScheme] {
  val schemeName = "fake"
  def read(config: Config, path: String): FakePowScheme = DefaultFakePowScheme
}
