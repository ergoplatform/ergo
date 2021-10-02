package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.PersistentNodeViewModifier
import scorex.core.transaction.state.MinimalState


trait SemanticallyInvalidModifierProducer[ST <: MinimalState[ST]] {
  def semanticallyInvalidModifier(state: ST): ErgoPersistentModifier
}
