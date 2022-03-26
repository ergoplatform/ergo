package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory


trait SyntacticallyTargetedModifierProducer {
  def syntacticallyValidModifier(history: ErgoHistory): ErgoPersistentModifier

  def syntacticallyInvalidModifier(history: ErgoHistory): ErgoPersistentModifier
}
