package scorex.testkit.generators

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.transaction.state.MinimalState


trait TotallyValidModifierProducer[ST <: MinimalState[ST]] {

  def totallyValidModifier(history: ErgoHistory, state: ST): ErgoPersistentModifier

  def totallyValidModifiers(history: ErgoHistory, state: ST, count: Int): Seq[ErgoPersistentModifier]
}
