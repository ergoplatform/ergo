package org.ergoplatform.nodeView.history.extra

import scorex.util.{ModifierId, bytesToId}

/**
 * Base trait for all additional indexes made by ExtraIndexer
 */
trait ExtraIndex {
  lazy val id: ModifierId = bytesToId(serializedId)
  def serializedId: Array[Byte]
}
