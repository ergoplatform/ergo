package org.ergoplatform.modifiers

import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, PersistentNodeViewModifier}

trait ErgoPersistentModifier extends PersistentNodeViewModifier {

  override def encodedId: String = Algos.encode(id)
}
