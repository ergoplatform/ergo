package org.ergoplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.modifiers.ModifierId

trait ModifierIdReader {

  implicit val modifierIdReader: ValueReader[ModifierId] = new ValueReader[ModifierId] {
    override def read(cfg: Config, path: String): ModifierId = {
      ModifierId.fromHex(cfg.getString(path))
    }
  }

}
