package org.ergoplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader
import scorex.util.ModifierId

trait ModifierIdReader {

  implicit val modifierIdReader: ValueReader[ModifierId] = new ValueReader[ModifierId] {
    override def read(cfg: Config, path: String): ModifierId = {
      ModifierId @@ cfg.getString(path)
    }
  }

}
