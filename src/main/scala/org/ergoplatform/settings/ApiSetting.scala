package org.ergoplatform.settings

sealed trait ApiSetting

case class ApiSettings(settings: Set[ApiSetting]) {
  def apply(setting: ApiSetting): Boolean = settings(setting)
}

object ApiSettings {
  def empty: ApiSettings = ApiSettings()

  def apply(pairs: (Option[Boolean], ApiSetting)*): ApiSettings = {
    val settings = pairs.collect {
      case (enabled, setting) if enabled.getOrElse(false) => setting
    }
    ApiSettings(settings.toSet)
  }

  object EstimateByteLength extends ApiSetting
}
