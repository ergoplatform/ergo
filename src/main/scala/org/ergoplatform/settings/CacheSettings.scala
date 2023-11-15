package org.ergoplatform.settings

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for different caches
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class CacheSettings(
  history: HistoryCacheSettings,
  network: NetworkCacheSettings,
  mempool: MempoolCacheSettings
)

case class HistoryCacheSettings(blockSectionsCacheSize: Int, extraCacheSize: Int, headersCacheSize: Int, indexesCacheSize: Int)

case class NetworkCacheSettings(
  invalidModifiersCacheSize: Int,
  invalidModifiersCacheExpiration: FiniteDuration
)

case class MempoolCacheSettings(
  invalidModifiersCacheSize: Int,
  invalidModifiersCacheExpiration: FiniteDuration
)
