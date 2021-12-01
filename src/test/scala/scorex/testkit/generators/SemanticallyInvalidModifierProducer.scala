package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.state.ErgoState


trait SemanticallyInvalidModifierProducer[ST <: ErgoState[ST]] {
  def semanticallyInvalidModifier(state: ST): ErgoPersistentModifier
}
