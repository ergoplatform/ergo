package org.ergoplatform.settings

import com.typesafe.config.ConfigException
import net.ceedubs.ficus.readers.ValueReader
import org.ergoplatform.nodeView.state._

trait StateTypeReaders {

  def stateTypeFromString(typeName: String, path: String): StateType = {
    StateType.values.find(_.stateTypeName == typeName)
      .getOrElse(throw new ConfigException.BadValue(path, typeName))
  }

}
