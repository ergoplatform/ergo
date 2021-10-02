package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.PersistentNodeViewModifier
import scorex.core.consensus.{History, SyncInfo}


trait SyntacticallyTargetedModifierProducer {
  def syntacticallyValidModifier(history: ErgoHistory): ErgoPersistentModifier

  def syntacticallyInvalidModifier(history: ErgoHistory): ErgoPersistentModifier
}
