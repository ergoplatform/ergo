package org.ergoplatform.modifiers

import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, PersistentNodeViewModifier}

trait ErgoPersistentModifier extends PersistentNodeViewModifier {


  //TODO Remove from base class
  def parentId: ModifierId = null  //scalastyle:ignore

  override def encodedId: String = Algos.encode(id)
}
