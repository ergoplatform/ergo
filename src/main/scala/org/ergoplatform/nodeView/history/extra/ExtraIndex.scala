package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.core.bytesToId
import org.ergoplatform.modifiers.ModifierId

/**
 * Base trait for all additional indexes made by ExtraIndexer
 */
trait ExtraIndex {
  lazy val id: ModifierId = bytesToId(serializedId)
  def serializedId: Array[Byte]
}
