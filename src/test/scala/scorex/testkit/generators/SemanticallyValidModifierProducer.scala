package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.transaction.state.MinimalState


trait SemanticallyValidModifierProducer[ST <: MinimalState[ST]] {
  def semanticallyValidModifier(state: ST): ErgoPersistentModifier
}


