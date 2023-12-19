package org.ergoplatform.settings

import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

/**
 * Settings related to headers-chain bootstrapping with NiPoPoWs. See ergo.node.nipopow section for settings description.
 */
case class NipopowSettings(nipopowBootstrap: Boolean, p2pNipopows: Int)

/**
 * Custom settings reader for `NipopowSettings`
 */
trait NipopowSettingsReader {
  implicit val nipopowSettingsReader: ValueReader[NipopowSettings] = { (cfg, path) =>
    NipopowSettings(
      cfg.as[Boolean](s"$path.nipopowBootstrap"),
      cfg.as[Int](s"$path.p2pNipopows")
    )
  }
}