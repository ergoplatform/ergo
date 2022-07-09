package scorex.testkit.generators

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistory


trait SyntacticallyTargetedModifierProducer {
  def syntacticallyValidModifier(history: ErgoHistory): BlockSection

  def syntacticallyInvalidModifier(history: ErgoHistory): BlockSection
}
