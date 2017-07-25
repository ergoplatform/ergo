package org.ergoplatform.nodeView.history

case class HistoryConfig(poPoWBootstrap: Boolean,
                         blocksToKeep: Int,
                         minimalSuffix: Int)
