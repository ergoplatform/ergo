package org.ergoplatform.settings

/**
  * Configuration file for different caches
  * @see src/main/resources/application.conf for parameters description
  */
case class CacheSettings(modifiersCacheSize: Int, indexesCacheSize: Int)

object CacheSettings {
  val default: CacheSettings = CacheSettings(100, 1000)
}