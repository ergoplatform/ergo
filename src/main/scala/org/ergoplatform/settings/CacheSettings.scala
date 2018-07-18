package org.ergoplatform.settings

/**
  * Configuration file for different caches
  * @see src/main/resources/application.conf for parameters description
  */
case class CacheSettings(historyStorageCacheSize: Int)

object CacheSettings {
  val default: CacheSettings = CacheSettings(100)
}