package scorex.core.utils

import ch.qos.logback.core.PropertyDefinerBase
import com.typesafe.config.ConfigFactory

class TypesafeConfigPropertyDefiner extends PropertyDefinerBase {
  private var propertyName = "scorex.logging.level"

  override def getPropertyValue: String = ConfigFactory.load.getString(propertyName)

  def setPropertyName(propertyName: String): Unit = {
    this.propertyName = propertyName
  }
}
