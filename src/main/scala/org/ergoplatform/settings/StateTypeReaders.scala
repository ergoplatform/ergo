package org.ergoplatform.settings

import com.typesafe.config.ConfigException
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.nodeView.state._

trait StateTypeReaders {

  implicit val stateTypeReader: ValueReader[StateType] = { (cfg, path) =>
    val typeKey = s"$path.stateType"
    val typeName = cfg.getString(typeKey)
    stateTypeFromString(typeName, typeKey)
  }

  def stateTypeFromString(typeName: String, path: String): StateType = {
    StateType.values.find(_.stateTypeName == typeName)
      .getOrElse(throw new ConfigException.BadValue(path, typeName))
  }

}
