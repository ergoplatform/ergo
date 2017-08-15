package org.ergoplatform.nodeView.history

import scala.concurrent.duration.FiniteDuration

case class HistoryConfig(blocksToKeep: Int,
                         minimalSuffix: Int,
                         blockInterval: FiniteDuration)
