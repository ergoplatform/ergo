package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.state.ErgoState

trait SemanticallyValidModifierProducer[ST <: ErgoState[ST]] {
  def semanticallyValidModifier(state: ST): ErgoPersistentModifier
}


