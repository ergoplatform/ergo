package org.ergoplatform.modifiers

import scorex.core.NodeViewModifier._
import scorex.core.PersistentNodeViewModifier

trait ErgoPersistentModifier extends PersistentNodeViewModifier {

  //TODO do we need version field for all modifiers?
  //val version: Version

  //TODO Remove from base class
  def parentId: ModifierId = null
}
